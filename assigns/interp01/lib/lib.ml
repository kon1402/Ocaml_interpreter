open Stdlib320
open Utils


(* parse function *)
let parse s = My_parser.parse s (*this is the parse function that uses the parser directly from the parser folder *)

(*s ubstitution helper function to be used in susbt*)
let rec occurs_free x e =
  match e with (* this checks if x occurs free in e and isn't unbound by like a let or fun *)
  | Num _ | True | False | Unit -> false
  | Var y -> x = y
  | Bop (_, e1, e2) -> occurs_free x e1 || occurs_free x e2
  | If (e1, e2, e3) -> occurs_free x e1 || occurs_free x e2 || occurs_free x e3
  | Let (y, e1, e2) -> occurs_free x e1 || (x <> y && occurs_free x e2)
  | Fun (y, e1) -> x <> y && occurs_free x e1 (* this is a recursive case for when y is found in e1. true if x is diff from y, false otherwise *)
  | App (e1, e2) -> occurs_free x e1 || occurs_free x e2

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
             subst v x (subst (VFun (fresh, Var fresh)) y e2))
  | Fun (y, e1) ->
      if x = y then
        Fun (y, e1)
      else if not (occurs_free x e1) then
        Fun (y, e1)
      else
        let fresh = gensym () in
        Fun (fresh, subst v x (subst (VFun (fresh, Var fresh)) y e1))
  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)

(* Evaluation function that uses subst in it for semantics *)
let rec eval (e : expr) : (value, error) result =
  match e with
  (* Base cases *)
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | Fun (x, e) -> Ok (VFun (x, e))
  
  (* Complex expressions *)
  | If (e1, e2, e3) ->
      (match eval e1 with
       | Ok (VBool true) -> eval e2
       | Ok (VBool false) -> eval e3
       | Ok _ -> Error InvalidIfCond
       | Error e -> Error e)
  
  | Bop (op, e1, e2)  ->
      (match op with
       | And ->  (* Short circuit AND *)
           (match eval e1 with
            | Ok (VBool false) -> Ok (VBool false)
            | Ok (VBool true) -> 
                (match eval e2 with
                 | Ok (VBool b) -> Ok (VBool b)
                 | Ok _ -> Error (InvalidArgs And)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs And)
            | Error e -> Error e)
       | Or ->   (* Short circuit OR *)
           (match eval e1 with
            | Ok (VBool true) -> Ok (VBool true)
            | Ok (VBool false) -> 
                (match eval e2 with
                 | Ok (VBool b) -> Ok (VBool b)
                 | Ok _ -> Error (InvalidArgs Or)
                 | Error e -> Error e)
            | Ok _ -> Error (InvalidArgs Or)
            | Error e -> Error e)
       | _ ->    (* Other binary operations *)
           match eval e1, eval e2 with
           | Ok v1, Ok v2 ->
               (match op, v1, v2 with
                | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
                | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
                | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
                | Div, VNum n1, VNum n2 -> 
                    if n2 = 0 then Error DivByZero
                    else Ok (VNum (n1 / n2))
                | Mod, VNum n1, VNum n2 ->
                    if n2 = 0 then Error DivByZero
                    else Ok (VNum (n1 mod n2))
                | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
                | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
                | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
                | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
                | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
                | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
                | _, _, _ -> Error (InvalidArgs op))
           | Error e, _ -> Error e
           | _, Error e -> Error e)
  
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
