
open Utils
(* Type Checker *)
let rec type_of e =
  match e with
  | Num _ -> Some TInt
  | Add (e1, e2) ->
      begin
        match type_of e1, type_of e2 with
        | Some TInt, Some TInt -> Some TInt
        | _ -> None
      end
  | Lt (e1, e2) ->
      begin
        match type_of e1, type_of e2 with
        | Some TInt, Some TInt -> Some TBool
        | _ -> None
      end
  | Ite (e1, e2, e3) ->
      begin
        match type_of e1, type_of e2, type_of e3 with
        | Some TBool, Some t2, Some t3 when t2 = t3 -> Some t2
        | _ -> None
      end
