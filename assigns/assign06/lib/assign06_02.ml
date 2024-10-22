
open Utils
let parse toks =
  let rec parse_helper toks stack =
    match toks with
    | [] -> (* base*)
        (* When no more tokens, check if the stack has exactly one expression *)
        begin match stack with
        | [expr] -> Some expr
        | _ -> None 
        end
    | tok :: rest -> (* the actual recursive case*)
        match tok with (* pattern match to check for either add, num, lt ot Ile*)
        | TNum n ->
            (* we push Num n onto the stack *)
            parse_helper rest (Num n :: stack)
        | TAdd ->
            (*pop two expressions and add the add to the stack*)
            begin match stack with
            | e2 :: e1 :: tail ->
                parse_helper rest (Add (e1, e2) :: tail)
            | _ ->
                (* Not enough expressions on the stack *)
                None
            end
        | TLt -> 
            (* Pop two expressions and create Lt *)
            begin match stack with
            | e2 :: e1 :: tail ->
                parse_helper rest (Lt (e1, e2) :: tail)
            | _ ->
                None
            end
        | TIte ->
            (* Pop three expressions and create Ite *)
            begin match stack with
            | e3 :: e2 :: e1 :: tail ->
                parse_helper rest (Ite (e1, e2, e3) :: tail)
            | _ ->
                None
            end
  in
  parse_helper toks [] (* Start with an empty stack for acc *)
