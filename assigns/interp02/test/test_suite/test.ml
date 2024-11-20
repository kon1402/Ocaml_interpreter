open OUnit2
open Utils
module S = Stdlib320  (* Alias Stdlib320 as S *)

(* Custom equality functions *)
let rec expr_equal e1 e2 =
  match e1, e2 with
  | Unit, Unit -> true
  | True, True -> true
  | False, False -> true
  | Num n1, Num n2 -> n1 = n2
  | Var x1, Var x2 -> x1 = x2
  | If (c1, t1, e1'), If (c2, t2, e2') ->
      expr_equal c1 c2 && expr_equal t1 t2 && expr_equal e1' e2'
  | Bop (op1, l1, r1), Bop (op2, l2, r2) ->
      op1 = op2 && expr_equal l1 l2 && expr_equal r1 r2
  | Fun (x1, t1, b1), Fun (x2, t2, b2) ->
      x1 = x2 && t1 = t2 && expr_equal b1 b2
  | App (f1, a1), App (f2, a2) ->
      expr_equal f1 f2 && expr_equal a1 a2
  | Let { is_rec = ir1; name = n1; ty = t1; value = v1; body = b1 },
    Let { is_rec = ir2; name = n2; ty = t2; value = v2; body = b2 } ->
      ir1 = ir2 && n1 = n2 && t1 = t2 && expr_equal v1 v2 && expr_equal b1 b2
  | Assert e1', Assert e2' -> expr_equal e1' e2'
  | _, _ -> false

let value_equal v1 v2 =
  match v1, v2 with
  | VUnit, VUnit -> true
  | VBool b1, VBool b2 -> b1 = b2
  | VNum n1, VNum n2 -> n1 = n2
  | VClos { arg = x1; body = b1; _ }, VClos { arg = x2; body = b2; _ } ->
      x1 = x2 && expr_equal b1 b2
  | _, _ -> false

let error_equal e1 e2 =
  match e1, e2 with
  | ParseErr, ParseErr -> true
  | UnknownVar x1, UnknownVar x2 -> x1 = x2
  | IfTyErr _, IfTyErr _ -> true
  | IfCondTyErr _, IfCondTyErr _ -> true
  | OpTyErrL (op1, _, _), OpTyErrL (op2, _, _) -> op1 = op2
  | OpTyErrR (op1, _, _), OpTyErrR (op2, _, _) -> op1 = op2
  | FunArgTyErr _, FunArgTyErr _ -> true
  | FunAppTyErr _, FunAppTyErr _ -> true
  | LetTyErr _, LetTyErr _ -> true
  | AssertTyErr _, AssertTyErr _ -> true
  | _, _ -> false

let result_equal r1 r2 =
  match r1, r2 with
  | S.Ok v1, S.Ok v2 -> value_equal v1 v2
  | S.Error e1, S.Error e2 -> error_equal e1 e2
  | _, _ -> false

(* Test function *)
let test s expected =
  let description = "Testing interp \"" ^ s ^ "\"" in
  let test_fun _ =
    let result = Lib.interp s in
    assert_bool description (result_equal result expected)
  in
  description >:: test_fun

(* Test suite *)
let basic_examples =
  "Basic interpreter examples" >::: [
    test "2 + 3" (S.Ok (VNum 5));
    test "true && false" (S.Ok (VBool false));
    test "let x = 5 in x" (S.Ok (VNum 5));
    test "if true then 1 else 0" (S.Ok (VNum 1));
    test "(fun x -> x + 1) 5" (S.Ok (VNum 6));
    test "let f = fun x -> x * x in f 3" (S.Ok (VNum 9));
    test "x + 1" (S.Error (UnknownVar "x"));
    test "let x = 5 in x x" (S.Error (FunAppTyErr IntTy));
    test "if true then false else" (S.Error ParseErr);
    test "let x = 1 in" (S.Error ParseErr);
    test "fun x ->" (S.Error ParseErr);
    test "(5) 3" (S.Error (FunAppTyErr IntTy));
    test "5 +" (S.Error ParseErr);
    test "if true then" (S.Error ParseErr);
    test "let in" (S.Error ParseErr);
    test "fun -> x" (S.Error ParseErr);
    (* Additional tests *)
    test "1 + true" (S.Error (OpTyErrR (Add, IntTy, BoolTy)));
    test "1 && false" (S.Error (OpTyErrL (And, BoolTy, IntTy)));
    test "assert 1" (S.Error (AssertTyErr IntTy));
  ]

let () = OUnit2.run_test_tt_main basic_examples
