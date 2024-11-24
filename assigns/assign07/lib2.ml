open Utils

(* Exception definitions *)
exception DivByZero
exception AssertFail

(* String conversion functions for debugging *)
let rec string_of_expr e =
  match e with
  | Unit -> "Unit"
  | True -> "True"
  | False -> "False"
  | Num n -> "Num " ^ string_of_int n
  | Var x -> "Var " ^ x
  | If (e1, e2, e3) ->
      "If (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | Bop (op, e1, e2) ->
      "Bop (" ^ string_of_bop op ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Fun (x, t, e) ->
      "Fun (" ^ x ^ ": " ^ string_of_ty t ^ ", " ^ string_of_expr e ^ ")"
  | App (e1, e2) ->
      "App (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Let { is_rec; name; ty; value; body } ->
      let rec_str = if is_rec then "rec " else "" in
      "Let (" ^ rec_str ^ name ^ ": " ^ string_of_ty ty ^ " = " ^
      string_of_expr value ^ " in " ^ string_of_expr body ^ ")"
  | Assert e ->
      "Assert (" ^ string_of_expr e ^ ")"

(* Removed 'rec' since the function is not recursive *)
let string_of_value v =
  match v with
  | VUnit -> "VUnit"
  | VBool b -> "VBool " ^ string_of_bool b
  | VNum n -> "VNum " ^ string_of_int n
  | VClos { name; arg; body = _; env = _ } ->
      let name_str = match name with Some n -> n | None -> "None" in
      "VClos { name = " ^ name_str ^ "; arg = " ^ arg ^ "; <function> }"


      
(* Parsing *)
let parse s = My_parser.parse s

(* Desugaring functions *)
let rec desugar_expr (e : sfexpr) : expr =
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg = (x, t); args = more_args; body } ->
      let base = Fun(x, t, desugar_expr body) in
      List.fold_right
        (fun (x, t) acc -> Fun(x, t, acc))
        more_args
        base
  | SApp (e1, e2) ->
      App(desugar_expr e1, desugar_expr e2)
  | SLet { is_rec; name; args; ty; value; body } ->
      let func_value = 
        match args with
        | [] -> desugar_expr value
        | _ ->
            List.fold_right
              (fun (x, t) acc -> Fun(x, t, acc))
              args
              (desugar_expr value)
      in
      Let {
        is_rec;
        name;
        ty;
        value = func_value;
        body = desugar_expr body
      }
  | SIf (e1, e2, e3) ->
      If(desugar_expr e1, desugar_expr e2, desugar_expr e3)
  | SBop (op, e1, e2) ->
      Bop(op, desugar_expr e1, desugar_expr e2)
  | SAssert e ->
      Assert(desugar_expr e)
(* Main desugaring function *)
let rec desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | decl :: rest ->
      (* Build function type correctly *)
      let func_ty = 
        if decl.args = [] then decl.ty
        else List.fold_right
          (fun (_, arg_ty) ret_ty -> FunTy(arg_ty, ret_ty))
          decl.args
          decl.ty 
      in
      (* Build function value correctly *)
      let func_value = 
        match decl.args with
        | [] -> desugar_expr decl.value
        | _ ->
            List.fold_right
              (fun (x, t) acc -> Fun(x, t, acc))
              decl.args
              (desugar_expr decl.value)
      in
      Let {
        is_rec = decl.is_rec;
        name = decl.name;
        ty = func_ty;
        value = func_value;
        body = desugar rest
      }
(* Type checking *)
let type_of (e : expr) : (ty, error) result =
  let rec type_of_env (env : ty Stdlib320.env) (e : expr) : (ty, error) result =
    match e with
    | Unit -> Ok UnitTy
    | True | False -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x ->
        (match Stdlib320.Env.find_opt x env with
         | Some t -> Ok t
         | None -> Error (UnknownVar x))
    | Fun (x, t1, e) ->
        let env' = Stdlib320.Env.add x t1 env in
        (match type_of_env env' e with
         | Ok t2 -> Ok (FunTy (t1, t2))
         | Error err -> Error err)
    | App (e1, e2) ->
        (match type_of_env env e1 with
         | Ok (FunTy (arg_ty, ret_ty)) ->
             (match type_of_env env e2 with
              | Ok t when t = arg_ty -> Ok ret_ty
              | Ok t ->
                  Error (FunArgTyErr (arg_ty, t))
              | Error err -> Error err)
         | Ok t -> Error (FunAppTyErr t)
         | Error err -> Error err)
    | If (e1, e2, e3) ->
        (match type_of_env env e1 with
         | Ok BoolTy ->
             (match type_of_env env e2, type_of_env env e3 with
              | Ok t2, Ok t3 when t2 = t3 -> Ok t2
              | Ok t2, Ok t3 -> Error (IfTyErr (t2, t3))
              | Error err, _ | _, Error err -> Error err)
         | Ok t -> Error (IfCondTyErr t)
         | Error err -> Error err)
    | Let { is_rec = false; name; ty; value; body } ->
        (match type_of_env env value with
         | Ok t when t = ty ->
             let env' = Stdlib320.Env.add name ty env in
             type_of_env env' body
         | Ok t -> Error (LetTyErr (ty, t))
         | Error err -> Error err)
    | Let { is_rec = true; name; ty; value; body } ->
        let env' = Stdlib320.Env.add name ty env in
        (match type_of_env env' value with
         | Ok t when t = ty ->
             type_of_env env' body
         | Ok t -> Error (LetTyErr (ty, t))
         | Error err -> Error err)
    | Bop (op, e1, e2) ->
        let expected_ty, result_ty =
          match op with
          | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy)
          | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, BoolTy)
          | And | Or -> (BoolTy, BoolTy)
        in
        (match type_of_env env e1 with
         | Ok t1 when t1 = expected_ty ->
             (match type_of_env env e2 with
              | Ok t2 when t2 = expected_ty -> Ok result_ty
              | Ok t2 ->
                  let error_msg = "Right operand of " ^ string_of_bop op ^ " has type " ^ string_of_ty t2 ^ ", expected " ^ string_of_ty expected_ty in
                  print_endline error_msg;
                  Error (OpTyErrR (op, expected_ty, t2))
              | Error err -> Error err)
         | Ok t1 ->
             let error_msg = "Left operand of " ^ string_of_bop op ^ " has type " ^ string_of_ty t1 ^ ", expected " ^ string_of_ty expected_ty in
             print_endline error_msg;
             Error (OpTyErrL (op, expected_ty, t1))
         | Error err -> Error err)
    | Assert e ->
        (match type_of_env env e with
         | Ok BoolTy -> Ok UnitTy
         | Ok t -> Error (AssertTyErr t)
         | Error err -> Error err)
  in
  type_of_env Stdlib320.Env.empty e

(* Evaluation *)
let eval (e : expr) : value =
  let rec eval_env (env : value Stdlib320.env) (e : expr) : value =
    match e with
    | Unit ->
        print_endline "Evaluating Unit";
        VUnit
    | True ->
        print_endline "Evaluating True";
        VBool true
    | False ->
        print_endline "Evaluating False";
        VBool false
    | Num n ->
        print_endline ("Evaluating Num: " ^ string_of_int n);
        VNum n
    | Var x ->
        print_endline ("Evaluating Var: " ^ x);
        (match Stdlib320.Env.find_opt x env with
         | Some v ->
             print_endline ("Found variable " ^ x ^ " with value " ^ string_of_value v);
             v
         | None ->
             print_endline ("Unbound variable: " ^ x);
             failwith ("Unbound variable: " ^ x))
    | Fun (x, _, body') ->
        print_endline ("Evaluating Fun: " ^ x);
        VClos { name = None; arg = x; body = body'; env }
    | App (e1, e2) ->
        print_endline "Evaluating App";
        let v1 = eval_env env e1 in
        let v2 = eval_env env e2 in
        print_endline ("Function: " ^ string_of_value v1);
        print_endline ("Argument: " ^ string_of_value v2);
        (match v1 with
         | VClos { name = None; arg; body; env = clos_env } ->
             let env' = Stdlib320.Env.add arg v2 clos_env in
             eval_env env' body
         | VClos { name = Some f; arg; body; env = clos_env } ->
             let closure = VClos { name = Some f; arg; body; env = clos_env } in
             let env' = Stdlib320.Env.add arg v2 (Stdlib320.Env.add f closure clos_env) in
             eval_env env' body
         | _ ->
             print_endline "Attempting to call a non-function value";
             failwith "Attempting to call a non-function value")
    | If (e1, e2, e3) ->
        print_endline "Evaluating If";
        (match eval_env env e1 with
         | VBool true ->
             print_endline "Condition is true";
             eval_env env e2
         | VBool false ->
             print_endline "Condition is false";
             eval_env env e3
         | _ ->
             print_endline "If condition must be a boolean";
             failwith "If condition must be a boolean")
   (* In eval_env function *)
| Let { is_rec = false; name; ty = _; value; body } ->
    print_endline ("Evaluating Let: " ^ name);
    let v = eval_env env value in
    let env' = Stdlib320.Env.add name v env in
    eval_env env' body

| Let { is_rec = true; name; ty = _; value; body } ->
    print_endline ("Evaluating Let Rec: " ^ name);
    (match value with
     | Fun (arg, _, body') ->
         let rec_closure = VClos { name = Some name; arg; body = body'; env } in
         let env' = Stdlib320.Env.add name rec_closure env in
         eval_env env' body
     | _ -> 
         print_endline "Let-rec must bind to a function";
         failwith "Let-rec must bind to a function")
    | Bop (op, e1, e2) ->
        print_endline ("Evaluating Bop: " ^ string_of_bop op);
        let v1 = eval_env env e1 in
        let v2 = eval_env env e2 in
        print_endline ("Operands: " ^ string_of_value v1 ^ ", " ^ string_of_value v2);
        (match (op, v1, v2) with
         | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
         | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
         | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
         | (Div, VNum n1, VNum n2) ->
             if n2 = 0 then raise DivByZero else VNum (n1 / n2)
         | (Mod, VNum n1, VNum n2) ->
             if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
         | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
         | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
         | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
         | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
         | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
         | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
         | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
         | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
         | _ ->
             print_endline "Invalid operands for binary operator";
             failwith "Invalid operands for binary operator")
    | Assert e ->
        print_endline "Evaluating Assert";
        (match eval_env env e with
         | VBool true ->
             print_endline "Assertion succeeded";
             VUnit
         | VBool false ->
             print_endline "Assertion failed";
             raise AssertFail
         | _ ->
             print_endline "Assert expression must evaluate to a boolean";
             failwith "Assert expression must evaluate to a boolean")
  in
  eval_env Stdlib320.Env.empty e

(* Interpreter *)
let interp (s : string) : (value, error) result =
  match parse s with
  | None ->
      print_endline "Parsing failed";
      Error ParseErr
  | Some prog ->
      print_endline "Parsing succeeded";
      let expr = desugar prog in
      print_endline ("Desugared expression: " ^ string_of_expr expr);
      match type_of expr with
      | Error e ->
          print_endline ("Type checking failed: " ^ err_msg e);
          Error e
      | Ok ty ->
          print_endline ("Type checking succeeded: " ^ string_of_ty ty);
          try
            let v = eval expr in
            print_endline ("Evaluation succeeded: " ^ string_of_value v);
            Ok v
          with
          | Failure msg ->
              print_endline ("Evaluation failed: " ^ msg);
              failwith msg
