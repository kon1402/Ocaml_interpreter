open Utils

exception AssertFail
exception DivByZero

let parse s = My_parser.parse s

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

      let rec desugar (p : prog) : expr =
        match p with
        | [] -> Unit  (* End of top-level declarations ends with unit *)
        | decl :: rest ->
            let func_ty = 
              if decl.args = [] then 
                (match decl.value with
                 | SFun { arg = (_, _); args = []; body } -> 
                     if decl.is_rec then
                       FunTy(IntTy, IntTy)  (* Recursive functions like factorial *)
                     else
                       FunTy(IntTy, infer_ty body)
                 | _ -> decl.ty)
              else 
                (* Convert multi-argument function type to curried form *)
                List.fold_right
                  (fun (_, arg_ty) ret_ty -> FunTy(arg_ty, ret_ty))
                  decl.args
                  decl.ty
            in
            let func_value = 
              match decl.args with
              | [] -> 
                  (match decl.value with
                   | SFun { arg = (x, _); args = []; body } ->
                       (* Single argument function *)
                       Fun(x, IntTy, desugar_expr body)
                   | _ -> desugar_expr decl.value)
              | _ ->
                  (* Convert multi-argument function to curried form *)
                  List.fold_right
                    (fun (x, t) acc -> Fun(x, t, acc))
                    decl.args
                    (desugar_expr decl.value)
            in
            (* Create nested let expressions *)
            Let {
              is_rec = decl.is_rec;
              name = decl.name;
              ty = func_ty;
              value = func_value;
              body = desugar rest  (* Recursively process remaining declarations *)
            }
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
              | Ok t -> Error (FunArgTyErr (arg_ty, t))
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
        let (expected_ty, result_ty) =
          match op with
          | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy)
          | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, BoolTy)
          | And | Or -> (BoolTy, BoolTy)
        in
        (match type_of_env env e1 with
         | Ok t1 when t1 = expected_ty ->
             (match type_of_env env e2 with
              | Ok t2 when t2 = expected_ty -> Ok result_ty
              | Ok t2 -> Error (OpTyErrR (op, expected_ty, t2))
              | Error err -> Error err)
         | Ok t1 -> Error (OpTyErrL (op, expected_ty, t1))
         | Error err -> Error err)
    | Assert e ->
        (match type_of_env env e with
         | Ok BoolTy -> Ok UnitTy
         | Ok t -> Error (AssertTyErr t)
         | Error err -> Error err)
  in
  type_of_env Stdlib320.Env.empty e

let eval (e : expr) : value =
  let rec eval_env (env : value Stdlib320.env) (e : expr) : value =
    match e with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x ->
        (match Stdlib320.Env.find_opt x env with
         | Some v -> v
         | None -> failwith ("Unbound variable: " ^ x))
    | Fun (x, _, body) ->
        VClos { name = None; arg = x; body; env }
    | App (e1, e2) ->
        let v1 = eval_env env e1 in
        let v2 = eval_env env e2 in
        (match v1 with
         | VClos { name = None; arg; body; env = clos_env } ->
             let env' = Stdlib320.Env.add arg v2 clos_env in
             eval_env env' body
         | VClos { name = Some f; arg; body; env = clos_env } ->
             let closure = VClos { name = Some f; arg; body; env = clos_env } in
             let env' = Stdlib320.Env.add arg v2 (Stdlib320.Env.add f closure clos_env) in
             eval_env env' body
         | _ -> failwith "Attempting to call a non-function value")
    | If (e1, e2, e3) ->
        (match eval_env env e1 with
         | VBool true -> eval_env env e2
         | VBool false -> eval_env env e3
         | _ -> failwith "If condition must be a boolean")
    | Let { is_rec = false; name; ty = _; value; body } ->
        let v = eval_env env value in
        let env' = Stdlib320.Env.add name v env in
        eval_env env' body
    | Let { is_rec = true; name; ty = _; value; body } ->
        (match value with
         | Fun (arg, _, body') ->
             let rec_closure = VClos { name = Some name; arg; body = body'; env } in
             let env' = Stdlib320.Env.add name rec_closure env in
             eval_env env' body
         | _ -> failwith "Let-rec must bind to a function")
    | Bop (op, e1, e2) ->
        let v1 = eval_env env e1 in
        let v2 = eval_env env e2 in
        (match (op, v1, v2) with
         | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
         | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
         | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
         | (Div, VNum n1, VNum n2) ->
             if n2 = 0 then raise DivByZero 
             else VNum (n1 / n2)
         | (Mod, VNum n1, VNum n2) ->
             if n2 = 0 then raise DivByZero 
             else VNum (n1 mod n2)
         | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
         | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
         | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
         | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
         | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
         | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
         | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
         | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
         | _ -> failwith "Invalid operands for binary operator")
    | Assert e ->
        (match eval_env env e with
         | VBool true -> VUnit
         | VBool false -> raise AssertFail
         | _ -> failwith "Assert expression must evaluate to a boolean")
  in
  eval_env Stdlib320.Env.empty e

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseErr
  | Some prog ->
      let expr = desugar prog in
      match type_of expr with
      | Error e -> Error e
      | Ok _ ->
          try Ok (eval expr)
          with
          | Failure msg -> failwith msg