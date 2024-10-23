
open Utils
let parse toks =
  let rec parse_helper toks stack =
    match toks with
    | [] -> (* base indicating the end of the tokens*)
        (* When no more tokens, check if the stack has exactly one expression *)
        begin
             match stack with (*pattern match to check if the stack was sucessfully built*)
        | [expr] -> Some expr (*the return end*)
        | _ -> None (*error checks. Hits this if there was an error running through and building the stack*)
        end
    | tok :: rest -> (* the actual recursive case*)
        match tok with (* pattern match to check for either add, num, lt ot Ile*)
        | TNum n ->
            (*we push Num n onto the stack *)
            parse_helper rest (Num n :: stack)
        | TAdd ->
            (*we pop two expressions and add the add to the stack*)
            begin 
                match stack with
            | e2 :: e1 :: tail ->
                parse_helper rest (Add (e1, e2) :: tail)
            | _ ->
                (*None*)
                None
            end
        | TLt -> 
            (*we pop two expressions and create Lt *)
            begin
                 match stack with
            | e2 :: e1 :: tail ->
                parse_helper rest (Lt (e1, e2) :: tail)
            | _ ->
                None
            end
        | TIte ->
            (*pop three expressions and create Ite *)
            begin
                match stack with
            | e3 :: e2 :: e1 :: tail ->
                parse_helper rest (Ite (e1, e2, e3) :: tail)
            | _ ->
                None
            end
  in
  parse_helper toks [] (*Start with an empty stack for acc *)
