
type ident = string

type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

(* Function to determine the type of an expression in a given context *)
let rec type_of' (gamma : context) (e : expr') : ty' option =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var x ->
      (* Lookup the variable in the context *)
      (try Some (List.assoc x gamma)
       with Not_found -> None)
  | Add (e1, e2) ->
      (* Both operands must be of type Int *)
      (match type_of' gamma e1, type_of' gamma e2 with
       | Some Int, Some Int -> Some Int
       | _, _ -> None)
  | Or (e1, e2) ->
      (* Both operands must be of type Bool *)
      (match type_of' gamma e1, type_of' gamma e2 with
       | Some Bool, Some Bool -> Some Bool
       | _, _ -> None)
  | IfThenElse (e1, e2, e3) ->
      (* Condition must be Bool; branches must have the same type *)
      (match type_of' gamma e1, type_of' gamma e2, type_of' gamma e3 with
       | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
       | _, _, _ -> None)
  | Let (x, e1, e2) ->
      (* Type-check e1, extend context, and type-check e2 *)
      (match type_of' gamma e1 with
       | Some t1 ->
           let gamma' = (x, t1) :: gamma in
           type_of' gamma' e2
       | None -> None)
