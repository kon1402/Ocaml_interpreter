(* Type definitions *)

type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

(* Function to determine the type of an expression in a given context *)
let rec type_of (gamma : ctxt) (e : expr) : ty option =
  match e with
  | Var x ->
      (* Lookup the variable in the context *)
      (try Some (List.assoc x gamma)
       with Not_found -> None)
  | Fun (x, ty_x, e_body) ->
      (* Extend the context with the new binding *)
      let gamma' = (x, ty_x) :: gamma in
      (* Type-check the body in the extended context *)
      (match type_of gamma' e_body with
       | Some ty_e_body ->
           (* The type of the function is ty_x → ty_e_body *)
           Some (Arr (ty_x, ty_e_body))
       | None -> None)
  | App (e1, e2) ->
      (* Type-check e1 and e2 *)
      (match type_of gamma e1, type_of gamma e2 with
       | Some (Arr (ty_arg, ty_res)), Some ty_e2 when ty_arg = ty_e2 ->
           (* e1 : ty_arg → ty_res, e2 : ty_arg *)
           Some ty_res
       | _, _ -> None)
