
open Assign04_02
(* Definition of val *)
type value = 
  | VNum of int
  | VBool of bool

(* Function to evaluate expressions *)
let rec eval e =
  match e with
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Add (e1, e2) ->
      (* Evaluate e1 and e2 *)
      let v1 = eval e1 in
      let v2 = eval e2 in
      (* Extract integer values and perform addition *)
      (match v1, v2 with
       | VNum n1, VNum n2 -> VNum (n1 + n2)
       | _ -> failwith "Undefined behavior: expected integer values in Add")
  | Or (e1, e2) ->
      (* Evaluate e1 and e2 *)
      let v1 = eval e1 in
      let v2 = eval e2 in
      (* Extract boolean values and perform logical OR *)
      (match v1, v2 with
       | VBool b1, VBool b2 -> VBool (b1 || b2)
       | _ -> failwith "Undefined behavior: expected boolean values in Or")
  | IfThenElse (e1, e2, e3) ->
      (* Evaluate the condition e1 *)
      let v1 = eval e1 in
      (match v1 with
       | VBool true -> eval e2
       | VBool false -> eval e3
       | _ -> failwith "Undefined behavior: expected boolean value in IfThenElse condition")
