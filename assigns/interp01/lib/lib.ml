
open Stdlib320
open Utils
(* parse function *)
let parse s = My_parser.parse s (*this is the parse function that uses the parser directly from the parser folder *)

(*s ub helper function to be used in susbt*)
let rec occurs_free x e =
  match e with (* this checks if x occurs free in e and isn't unbound by like a let or fun *)
  | Num _ | True | False | Unit -> false
  | Var y -> x = y
  | Bop (_, e1, e2) -> occurs_free x e1 || occurs_free x e2
  | If (e1, e2, e3) -> occurs_free x e1 || occurs_free x e2 || occurs_free x e3
  | Let (y, e1, e2) -> occurs_free x e1 || (x <> y && occurs_free x e2)
  | Fun (y, e1) -> x <> y && occurs_free x e1 (* this is a recursive case for when y is found in e1. true if x is diff from y, false otherwise *)
  | App (e1, e2) -> occurs_free x e1 || occurs_free x e2


(* subs the variable x with variable y in expression e *)
let rec subst_var y x e =
  match e with
  | Var z -> if z = x then Var y else Var z
  | Num n -> Num n
  | True -> True
  | False -> False
  | Unit -> Unit
  | Bop (op, e1, e2) -> Bop (op, subst_var y x e1, subst_var y x e2)
  | If (e1, e2, e3) -> If (subst_var y x e1, subst_var y x e2, subst_var y x e3)
  | Let (z, e1, e2) ->
      if z = x then
        Let (z, subst_var y x e1, e2)  (* x is shadowed *)
      else
        Let (z, subst_var y x e1, subst_var y x e2)
  | Fun (z, e1) ->
      if z = x then
        Fun (z, e1)  (* x is shadowed *)
      else
        Fun (z, subst_var y x e1)
  | App (e1, e2) -> App (subst_var y x e1, subst_var y x e2)

(* substitution function for the eval part *)
let rec subst v x e =
  match e with
  | Num n -> Num n
  | True -> True
  | False -> False
  | Unit -> Unit
  | Var y -> 
      if x = y then 
        (match v with
        | VNum n -> Num n
        | VBool b -> if b then True else False
        | VUnit -> Unit
        | VFun (param, body) -> Fun (param, body))
      else Var y
  | Bop (op, e1, e2) ->
      Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) ->
      If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      if x = y then
        (* x is shadowed, only substitute in e1 *)
        Let (y, subst v x e1, e2)
      else if not (occurs_free x e2) then
        Let (y, subst v x e1, e2)
      else
        let fresh = gensym () in
        Let (fresh, 
             subst v x e1,
             subst v x (subst_var fresh y e2))
  | Fun (y, e1) ->
      if x = y then
        Fun (y, e1)
      else if not (occurs_free x e1) then
        Fun (y, e1)
      else
        let fresh = gensym () in
        Fun (fresh, subst v x (subst_var fresh y e1))
  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)


(* Evaluation function that uses subst in it for semantics *)
(* Evaluation function *)
let rec eval e =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | Fun (x, e) -> Ok (VFun (x, e))
  | If (e1, e2, e3) ->
      (match eval e1 with
       | Ok (VBool true) -> eval e2
       | Ok (VBool false) -> eval e3
       | Ok _ -> Error InvalidIfCond
       | Error e -> Error e)
  
  | Bop (op, e1, e2) ->
      (match op with
       | And ->  (* Short-circuit AND *)
           (match eval e1 with
            | Ok (VBool false) -> Ok (VBool false)
            | Ok (VBool true) ->
                (match eval e2 with
                 | Ok (VBool b) -> Ok (VBool b)
                 | Ok _ -> Error (InvalidArgs And)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs And)
            | Error e -> Error e)
       | Or ->   (* Short-circuit OR *)
           (match eval e1 with
            | Ok (VBool true) -> Ok (VBool true)
            | Ok (VBool false) ->
                (match eval e2 with
                 | Ok (VBool b) -> Ok (VBool b)
                 | Ok _ -> Error (InvalidArgs Or)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs Or)
            | Error e -> Error e)
       | Eq | Neq ->
           (match eval e1 with
            | Ok v1 ->
                (match eval e2 with
                 | Ok v2 ->
                     (match v1, v2 with
                      | VNum n1, VNum n2 ->
                          if op = Eq then Ok (VBool (n1 = n2)) else Ok (VBool (n1 <> n2))
                      | VBool b1, VBool b2 ->
                          if op = Eq then Ok (VBool (b1 = b2)) else Ok (VBool (b1 <> b2))
                      | VUnit, VUnit ->
                          if op = Eq then Ok (VBool true) else Ok (VBool false)
                      | VFun _, VFun _ ->
                          Error (InvalidArgs op)  (*the functions are not comparable *)
                      | _, _ ->
                          Error (InvalidArgs op))
                 | Error e -> Error e)
            | Error e -> Error e)
       | Lt | Lte | Gt | Gte ->
           (match eval e1 with
            | Ok (VNum n1) ->
                (match eval e2 with
                 | Ok (VNum n2) ->
                     let result = match op with
                       | Lt -> n1 < n2
                       | Lte -> n1 <= n2
                       | Gt -> n1 > n2
                       | Gte -> n1 >= n2
                       | _ -> false  (*false case (potential edge case?) *)
                     in Ok (VBool result)
                 | Ok _ -> Error (InvalidArgs op)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs op)
            | Error e -> Error e)
       | Add | Sub | Mul | Div | Mod ->
           (match eval e1 with
            | Ok (VNum n1) ->
                (match eval e2 with
                 | Ok (VNum n2) ->
                     (match op with
                      | Add -> Ok (VNum (n1 + n2))
                      | Sub -> Ok (VNum (n1 - n2))
                      | Mul -> Ok (VNum (n1 * n2))
                      | Div ->
                          if n2 = 0 then Error DivByZero
                          else Ok (VNum (n1 / n2))
                      | Mod ->
                          if n2 = 0 then Error DivByZero
                          else Ok (VNum (n1 mod n2))
                      | _ -> Error (InvalidArgs op))
                 | Ok _ -> Error (InvalidArgs op)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs op)
            | Error e -> Error e)
      )
  
  | Let (x, e1, e2) ->
      (match eval e1 with
       | Ok v1 -> eval (subst v1 x e2)
       | Error e -> Error e)
  
  | App (e1, e2) ->
      (match eval e1 with
       | Ok (VFun (x, e)) ->
           (match eval e2 with
            | Ok v -> eval (subst v x e)
            | Error e -> Error e)
       | Ok _ -> Error InvalidApp
       | Error e -> Error e)


(* Interpreter function *)
let interp s =
  match parse s with
  | None -> Error ParseFail
  | Some prog -> eval prog
