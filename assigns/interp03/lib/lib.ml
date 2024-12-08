open Utils
include My_parser

(* Required exceptions *)
exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

(* Helper function for fresh type variables *)
let gensym () = TVar (Stdlib320.gensym ())

(* Substitute type variables in a type *)
let rec substitute_ty subst ty =
  match ty with
  | TUnit | TInt | TFloat | TBool -> ty
  | TVar x -> (match List.assoc_opt x subst with Some t -> t | None -> ty)
  | TList t -> TList (substitute_ty subst t)
  | TOption t -> TOption (substitute_ty subst t)
  | TPair (t1, t2) -> TPair (substitute_ty subst t1, substitute_ty subst t2)
  | TFun (t1, t2) -> TFun (substitute_ty subst t1, substitute_ty subst t2)

let substitute_constr subst (t1, t2) =
  (substitute_ty subst t1, substitute_ty subst t2)

(* Free type variables in a type *)
let rec free_vars_ty ty =
  match ty with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList t -> free_vars_ty t
  | TOption t -> free_vars_ty t
  | TPair (t1,t2) | TFun (t1,t2) ->
    VarSet.union (free_vars_ty t1) (free_vars_ty t2)

(* Compute free vars in environment *)
let free_vars_env env =
  let bindings = Env.to_list env in
  List.fold_left (fun acc (_, Forall(vs,ty)) ->
    let fv = free_vars_ty ty in
    (* remove vs from fv *)
    let fv_list = VarSet.to_list fv in
    let fv_filtered = List.filter (fun x -> not (List.mem x vs)) fv_list in
    VarSet.union acc (VarSet.of_list fv_filtered)
  ) VarSet.empty bindings

let unify ty constrs =
  let rec occurs x = function
    | TUnit | TInt | TFloat | TBool -> false
    | TVar y -> x = y
    | TList t -> occurs x t
    | TOption t -> occurs x t
    | TPair(t1,t2) | TFun(t1,t2) -> occurs x t1 || occurs x t2
  in
  let rec solve subst = function
    | [] -> Some subst
    | (t1,t2)::rest when t1 = t2 -> solve subst rest
    | (TVar x,t)::rest | (t,TVar x)::rest ->
        if occurs x t then None else
        let subst' = (x,t)::subst in
        let rest' = List.map (substitute_constr [(x,t)]) rest in
        solve subst' rest'
    | (TList t1,TList t2)::rest ->
        solve subst ((t1,t2)::rest)
    | (TOption t1,TOption t2)::rest ->
        solve subst ((t1,t2)::rest)
    | (TPair(t11,t12),TPair(t21,t22))::rest ->
        solve subst ((t11,t21)::(t12,t22)::rest)
    | (TFun(t11,t12),TFun(t21,t22))::rest ->
        solve subst ((t11,t21)::(t12,t22)::rest)
    | _ -> None
  in
  match solve [] constrs with
  | None -> None
  | Some subst ->
      let ty' = substitute_ty subst ty in
      let fv = free_vars_ty ty' in
      let fv_list = VarSet.to_list fv in
      Some (Forall(fv_list, ty'))

let type_of env expr =
  let rec infer env expr = match expr with
    | Unit -> Ok (TUnit, [], VarSet.empty)
    | True | False -> Ok (TBool, [], VarSet.empty)
    | Int _ -> Ok (TInt, [], VarSet.empty)
    | Float _ -> Ok (TFloat, [], VarSet.empty)
    | Nil ->
        let alpha = gensym () in Ok (TList alpha, [], VarSet.empty)
    | ENone ->
        let alpha = gensym () in Ok (TOption alpha, [], VarSet.empty)
    | ESome e ->
        (match infer env e with
         | Ok(t,c,v) -> Ok(TOption t,c,v)
         | Error e->Error e)
    | Var x ->
        (match Env.find_opt x env with
         | Some (Forall(vars, ty)) ->
             let subst = List.map (fun v -> (v, gensym ())) vars in
             let t' = substitute_ty subst ty in
             Ok (t',[],VarSet.empty)
         | None -> Error TypeError)
    | Fun(x,Some t,e) ->
        let env' = Env.add x (Forall([],t)) env in
        (match infer env' e with
         | Ok(t2,c,v)->Ok(TFun(t,t2),c,v)
         | Error e->Error e)
    | Fun(x,None,e) ->
        let alpha = gensym () in
        let env' = Env.add x (Forall([],alpha)) env in
        (match infer env' e with
         | Ok(t,c,v)->Ok(TFun(alpha,t),c,v)
         | Error e->Error e)
    | App(e1,e2) ->
        let alpha = gensym () in
        (match infer env e1, infer env e2 with
         | Ok(t1,c1,v1),Ok(t2,c2,v2)->
             Ok(alpha,(t1,TFun(t2,alpha))::(c1@c2),VarSet.union v1 v2)
         | Error e,_|_,Error e->Error e)
    | If(e1,e2,e3) ->
        (match infer env e1, infer env e2, infer env e3 with
         | Ok(t1,c1,v1),Ok(t2,c2,v2),Ok(t3,c3,v3)->
             let v=VarSet.union v1 (VarSet.union v2 v3) in
             Ok(t2,(t1,TBool)::(t2,t3)::(c1@c2@c3),v)
         | Error e,_,_ | _,Error e,_ | _,_,Error e->Error e)
    | Let{is_rec=false;name;value;body} ->
        (match infer env value with
         | Ok(t1,c1,v1)->
             let env' = Env.add name (Forall([],t1)) env in
             (match infer env' body with
              | Ok(t2,c2,v2)->Ok(t2,c1@c2,VarSet.union v1 v2)
              | Error e->Error e)
         | Error e->Error e)
    | Let{is_rec=true;name;value;body} ->
        let alpha = gensym () in
        let beta = gensym () in
        let env' = Env.add name (Forall([],TFun(alpha,beta))) env in
        (match infer env' value with
         | Ok(t1,c1,v1)->
             let c_new = (t1,TFun(alpha,beta))::c1 in
             (match infer env' body with
              | Ok(t2,c2,v2)->
                  Ok(t2,c_new@c2,VarSet.union v1 v2)
              | Error e->Error e)
         | Error e->Error e)
    | Annot(e, ty) ->
        (match infer env e with
         | Ok(t,c,v)->Ok(ty,(t,ty)::c,v)
         | Error e->Error e)
    | Assert e ->
        (match e with
         | False ->
             let alpha = gensym () in
             Ok(alpha,[],VarSet.empty)
         | _ ->
           (match infer env e with
            | Ok(t,c,v)->Ok(TUnit,(t,TBool)::c,v)
            | Error e->Error e))
    | Bop(op,e1,e2) ->
        (match infer env e1, infer env e2 with
         | Ok(t1,c1,v1),Ok(t2,c2,v2)->
             let v=VarSet.union v1 v2 in
             let (op_constrs,result_ty)=
               match op with
               | Add|Sub|Mul|Div|Mod-> ([(t1,TInt);(t2,TInt)],TInt)
               | AddF|SubF|MulF|DivF|PowF-> ([(t1,TFloat);(t2,TFloat)],TFloat)
               | Lt|Lte|Gt|Gte|Eq|Neq ->
                   (* both sides same type; no restriction beyond equality *)
                   ([(t1,t2)],TBool)
               | And|Or-> ([(t1,TBool);(t2,TBool)],TBool)
               | Cons->
                   let a=gensym() in ([(t1,a);(t2,TList a)],TList a)
               | Concat->
                   let a=gensym() in ([(t1,TList a);(t2,TList a)],TList a)
               | Comma->
                   let a=gensym() in let b=gensym() in ([(t1,a);(t2,b)],TPair(a,b))
             in Ok(result_ty,op_constrs@c1@c2,v)
         | Error e,_|_,Error e->Error e)
    | ListMatch{matched;nil_case;hd_name;tl_name;cons_case} ->
        let alpha = gensym () in
        (match infer env matched with
         | Ok(t1,c1,v1)->
             let env' = Env.add hd_name (Forall([],alpha))
                        (Env.add tl_name (Forall([],TList alpha)) env) in
             (match infer env nil_case, infer env' cons_case with
              | Ok(t2,c2,v2),Ok(t3,c3,v3)->
                  let v=VarSet.union v1 (VarSet.union v2 v3) in
                  Ok(t2,(t1,TList alpha)::(t2,t3)::(c1@c2@c3),v)
              | Error e,_|_,Error e->Error e)
         | Error e->Error e)
    | OptMatch{matched;some_name;some_case;none_case} ->
        let alpha=gensym() in
        (match infer env matched with
         | Ok(t1,c1,v1)->
             let env' = Env.add some_name (Forall([],alpha)) env in
             (match infer env' some_case, infer env none_case with
              | Ok(t2,c2,v2),Ok(t3,c3,v3)->
                  let v=VarSet.union v1 (VarSet.union v2 v3) in
                  Ok(t2,(t1,TOption alpha)::(t2,t3)::(c1@c2@c3),v)
              | Error e,_|_,Error e->Error e)
         | Error e->Error e)
    | PairMatch{matched;fst_name;snd_name;case} ->
        let alpha=gensym() in
        let beta=gensym() in
        (match infer env matched with
         | Ok(t1,c1,v1)->
             let env' = Env.add fst_name (Forall([],alpha))
                        (Env.add snd_name (Forall([],beta)) env) in
             (match infer env' case with
              | Ok(t2,c2,v2)->
                  let v=VarSet.union v1 v2 in
                  Ok(t2,(t1,TPair(alpha,beta))::c1@c2,v)
              | Error e->Error e)
         | Error e->Error e)
  in
  match infer env expr with
  | Ok(ty,c,_) ->
      (match unify ty c with
       | None->None
       | Some(Forall(fv_list,t))->
           let env_vars = free_vars_env env in
           let final_fv_list =
             List.filter (fun x -> not (VarSet.mem x env_vars)) fv_list
           in
           Some(Forall(final_fv_list,t))
      )
  | Error _->None

let rec eval_expr env expr =
  match expr with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Nil -> VList []
  | ENone -> VNone
  | ESome e -> VSome(eval_expr env e)
  | Var x ->
      (match Env.find_opt x env with
       | Some v->v
       | None->failwith ("Unbound variable: "^x))
  | Fun(arg,_,body)-> VClos{name=None;arg;body;env}
  | App(e1,e2)->
      (match eval_expr env e1 with
       | VClos{name;arg;body;env=env'}->
           let v=eval_expr env e2 in
           let env''=match name with
             | Some f->Env.add f (VClos{name;arg;body;env=env'}) env'
             | None->env'
           in eval_expr (Env.add arg v env'') body
       | _->failwith "Application of non-function value")
  | If(e1,e2,e3)->
      (match eval_expr env e1 with
       | VBool true->eval_expr env e2
       | VBool false->eval_expr env e3
       | _->failwith "If condition must be a boolean")
  | Let{is_rec=false;name;value;body} ->
      let v=eval_expr env value in
      let env'=Env.add name v env in
      eval_expr env' body
  | Let{is_rec=true;name;value;body} ->
      (match value with
       | Fun(arg,_,body')->
           let rec_closure=VClos{name=Some name;arg;body=body';env} in
           let env'=Env.add name rec_closure env in
           eval_expr env' body
       | _->raise RecWithoutArg)
  | Bop(op,e1,e2)->
      let v1=eval_expr env e1 in
      let v2=eval_expr env e2 in
      (match (op,v1,v2) with
       |(Add,VInt n1,VInt n2)->VInt(n1+n2)
       |(Sub,VInt n1,VInt n2)->VInt(n1-n2)
       |(Mul,VInt n1,VInt n2)->VInt(n1*n2)
       |(Div,VInt n1,VInt n2)->
         if n2=0 then raise DivByZero else VInt(n1/n2)
       |(Mod,VInt n1,VInt n2)->
         if n2=0 then raise DivByZero else VInt(n1 mod n2)
       |(AddF,VFloat n1,VFloat n2)->VFloat(n1+.n2)
       |(SubF,VFloat n1,VFloat n2)->VFloat(n1-.n2)
       |(MulF,VFloat n1,VFloat n2)->VFloat(n1*.n2)
       |(DivF,VFloat n1,VFloat n2)->VFloat(n1/.n2)
       |(PowF,VFloat n1,VFloat n2)->VFloat(n1**n2)
       |(Cons,v1,VList vs)->VList(v1::vs)
       |(Concat,VList l1,VList l2)->VList(l1@l2)
       |(Lt,VClos _,_)|(Lt,_,VClos _)
       |(Lte,VClos _,_)|(Lte,_,VClos _)
       |(Gt,VClos _,_)|(Gt,_,VClos _)
       |(Gte,VClos _,_)|(Gte,_,VClos _)
       |(Eq,VClos _,_)|(Eq,_,VClos _)
       |(Neq,VClos _,_)|(Neq,_,VClos _) -> raise CompareFunVals
       |(Lt,_,_)->VBool(v1<v2)
       |(Lte,_,_)->VBool(v1<=v2)
       |(Gt,_,_)->VBool(v1>v2)
       |(Gte,_,_)->VBool(v1>=v2)
       |(Eq,_,_)->VBool(v1=v2)
       |(Neq,_,_)->VBool(v1<>v2)
       |(And,VBool b1,VBool b2)->VBool(b1&&b2)
       |(Or,VBool b1,VBool b2)->VBool(b1||b2)
       |(Comma,v1,v2)->VPair(v1,v2)
       |_->failwith "Invalid operand types for binary operation")
  | Assert e->
      (match e with
       | False -> VUnit
       | _ ->
         (match eval_expr env e with
          | VBool true->VUnit
          | VBool false->raise AssertFail
          |_ -> failwith "Assert expression must evaluate to a boolean"))
  | ListMatch{matched;nil_case;hd_name;tl_name;cons_case} ->
      (match eval_expr env matched with
       | VList []->eval_expr env nil_case
       | VList(hd::tl)->
         let env'=Env.add hd_name hd (Env.add tl_name (VList tl) env) in
         eval_expr env' cons_case
       | _->failwith "Match expression expected a list")
  | OptMatch{matched;none_case;some_name;some_case} ->
      (match eval_expr env matched with
       | VNone->eval_expr env none_case
       | VSome v->
         let env' = Env.add some_name v env in
         eval_expr env' some_case
       | _->failwith "Match expression expected an option")
  | PairMatch{matched;fst_name;snd_name;case} ->
      (match eval_expr env matched with
       | VPair(v1,v2)->
         let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
         eval_expr env' case
       | _->failwith "Match expression expected a pair")
  | Annot(e,_) -> eval_expr env e

let type_check prog =
  let rec go ctxt = function
    | [] -> Some (Forall ([], TUnit))
    | {is_rec;name;value}::ls ->
      match type_of ctxt (Let {is_rec;name;value;body=Var name}) with
      | Some ty ->
          (match ls with
           | []->Some ty
           | _->
             let ctxt = Env.add name ty ctxt in
             go ctxt ls)
      | None->None
  in go Env.empty prog

let eval p =
  let rec nest = function
    | []->Unit
    | [{is_rec;name;value}] -> Let{is_rec;name;value;body=Var name}
    | {is_rec;name;value}::ls->Let{is_rec;name;value;body=nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->(
    match type_check prog with
    | Some ty->Ok(eval prog, ty)
    | None->Error TypeError
  )
  | None->Error ParseError
