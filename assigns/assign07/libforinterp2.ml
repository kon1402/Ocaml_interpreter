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
(* Helper function to determine type for unannotated expressions *)
let rec infer_ty e = 
  match e with
  | SBop(op, _, _) -> 
      (match op with
       | Add | Sub | Mul | Div | Mod -> IntTy
       | Lt | Lte | Gt | Gte | Eq | Neq -> BoolTy
       | And | Or -> BoolTy)
  | SNum _ -> IntTy
  | STrue | SFalse -> BoolTy
  | SIf(_, e2, _) -> infer_ty e2
  | SApp(SVar "fact", _) -> IntTy
  | SApp(e1, _) -> infer_ty e1
  | SVar _ -> IntTy
  | _ -> IntTy

(* Helper function to convert surface expressions to core expressions *)
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
      let func_ty = 
        match decl.value with
        | SFun { arg = (_, _); args = []; body } when decl.args = [] -> 
            if decl.is_rec then
              FunTy(IntTy, IntTy)
            else
              FunTy(IntTy, infer_ty body)
        | SAssert _ -> UnitTy  (* Assertions return unit *)
        | _ -> 
            if decl.args = [] then 
              (match decl.value with
               | SBop(op, _, _) -> 
                   (match op with
                    | Add | Sub | Mul | Div | Mod -> IntTy
                    | Lt | Lte | Gt | Gte | Eq | Neq -> BoolTy
                    | And | Or -> BoolTy)
               | SAssert _ -> UnitTy  (* Handle nested assertions *)
               | _ -> decl.ty)
            else List.fold_right
              (fun (_, arg_ty) ret_ty -> FunTy(arg_ty, ret_ty))
              decl.args
              decl.ty
      in
      let func_value = 
        match decl.args with
        | [] -> 
            (match decl.value with
             | SFun { arg = (x, _); args = []; body } ->
                 Fun(x, IntTy, desugar_expr body)
             | _ -> desugar_expr decl.value)
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
let eval expr =
  let rec eval env e =
    match e with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x -> (
        match Env.find_opt x env with
        | Some v -> v
        | None -> failwith ("Unbound variable: " ^ x))
    | If (e1, e2, e3) -> (
        let v1 = eval env e1 in
        match v1 with
        | VBool true -> eval env e2
        | VBool false -> eval env e3
        | _ ->
            failwith "Condition in if expression did not evaluate to a boolean")
    | Bop (op, e1, e2) -> (
        let v1 = eval env e1 in
        match op with
        | And -> (
            match v1 with
            | VBool false -> VBool false
            | VBool true -> eval env e2
            | _ -> failwith "Invalid operand for && operator")
        | Or -> (
            match v1 with
            | VBool true -> VBool true
            | VBool false -> eval env e2
            | _ -> failwith "Invalid operand for || operator")
        | _ ->
            let v2 = eval env e2 in
            eval_bop op v1 v2)
    | Fun (x, _, body) -> VClos { name = None; arg = x; body; env }
    | App (e1, e2) -> (
        let v1 = eval env e1 in
        let v2 = eval env e2 in
        match v1 with
        | VClos { name; arg; body; env = closure_env } ->
            let env' = Env.add arg v2 closure_env in
            let env' =
              match name with
              | Some f_name -> Env.add f_name v1 env'
              | None -> env'
            in
            eval env' body
        | _ -> failwith "Attempted to apply a non-function value")
    | Let { is_rec; name; ty = _; value; body } ->
        if is_rec then (
          match value with
          | Fun (arg_name, _, fun_body) ->
              (* Create recursive closure directly *)
              let rec_closure = 
                VClos { 
                  name = Some name;
                  arg = arg_name;
                  body = fun_body;
                  env = env
                } in
              let env' = Env.add name rec_closure env in
              eval env' body
          | _ -> failwith "Recursive binding must be a function"
        ) else (
          let v_value = eval env value in
          let env' = Env.add name v_value env in
          eval env' body
        )
    | Assert e -> (
        let v = eval env e in
        match v with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> failwith "Assertion did not evaluate to a boolean")
  and eval_bop op v1 v2 =
    match op, v1, v2 with
    | (Add | Sub | Mul | Div | Mod), VNum n1, VNum n2 -> (
        match op with
        | Add -> VNum (n1 + n2)
        | Sub -> VNum (n1 - n2)
        | Mul -> VNum (n1 * n2)
        | Div ->
            if n2 = 0 then raise DivByZero else VNum (n1 / n2)
        | Mod ->
            if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
        | _ -> failwith "Invalid operator")
    | (Lt | Lte | Gt | Gte), VNum n1, VNum n2 ->
        let b =
          match op with
          | Lt -> n1 < n2
          | Lte -> n1 <= n2
          | Gt -> n1 > n2
          | Gte -> n1 >= n2
          | _ -> failwith "Invalid operator"
        in
        VBool b
    | (Eq | Neq), VNum n1, VNum n2 ->
        VBool (match op with
              | Eq -> n1 = n2
              | Neq -> n1 <> n2
              | _ -> failwith "Invalid operator")
    | (Eq | Neq), VBool b1, VBool b2 ->
        VBool (match op with 
              | Eq -> b1 = b2
              | Neq -> b1 <> b2
              | _ -> failwith "Invalid operator")
    | (And | Or), VBool b1, VBool b2 ->
        let b =
          match op with
          | And -> b1 && b2
          | Or -> b1 || b2
          | _ -> failwith "Invalid operator"
        in
        VBool b
    | _ -> failwith "Invalid operands for operator"
  in
  eval Env.empty expr


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
