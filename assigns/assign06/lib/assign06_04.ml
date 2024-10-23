
open Utils
let rec eval e =
  match e with
  | Num n ->
      VNum n
  | Add (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      begin
        match (v1, v2) with
        | (VNum n1, VNum n2) -> VNum (n1 + n2)
        | _ -> failwith "Type error: Add expects two numbers lol"
      end
  | Lt (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      begin (*used begin and end to do nested matching*)
        match (v1, v2) with
        | (VNum n1, VNum n2) -> VBool (n1 < n2)
        | _ -> failwith "Type error: Lt expects twO numbers"
      end
  | Ite (e1, e2, e3) ->
      let v = eval e1 in
      begin (*same*)
        match v with
        | VBool b ->
            if b then eval e2 else eval e3
        | _ -> failwith "Type error: Ite expects a boolean condition"
      end
