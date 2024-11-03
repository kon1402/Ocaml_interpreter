open OUnit2
open Utils
open Lib

(* Existing parse testing helper *)
let test_parse s e =
  let d = "testing parse \"" ^ s ^ "\"" in
  let t _ =
    let a = My_parser.parse s in
    assert_equal e a
  in d >: test_case ~length:(Custom_length 2.) t

(* New helper for testing evaluation *)
let test_eval e expected =
  let d = "testing eval " in
  let t _ =
    match eval e with
    | Ok v -> assert_equal expected v
    | Error e -> assert_failure (err_msg e)
  in d >: test_case t

(* New helper for testing interpretation *)
let test_interp s expected =
  let d = "testing interp \"" ^ s ^ "\"" in
  let t _ =
    match interp s with
    | Ok v -> assert_equal expected v
    | Error e -> assert_failure (err_msg e)
  in d >: test_case t

(* Your existing parse tests *)
let basic_examples = "basic parse examples" >::: [
  (* ... your existing parse tests ... *)
]

(* New evaluation tests *)
let eval_tests = "eval tests" >::: [
  test_eval (Num 42) (VNum 42);
  test_eval True (VBool true);
  test_eval False (VBool false);
  test_eval Unit VUnit;
  test_eval (Bop (Add, Num 2, Num 3)) (VNum 5);
  test_eval (If (True, Num 1, Num 0)) (VNum 1);
  test_eval (Fun ("x", Var "x")) (VFun ("x", Var "x"))
]

(* New interpretation tests *)
let interp_tests = "interp tests" >::: [
  test_interp "42" (VNum 42);
  test_interp "true" (VBool true);
  test_interp "2 + 3" (VNum 5);
  test_interp "if true then 1 else 0" (VNum 1);
  test_interp "let x = 5 in x" (VNum 5);
  test_interp "fun x -> x" (VFun ("x", Var "x"))
]

(* Combine all test suites *)
let suite = "all tests" >::: [
  basic_examples;
  eval_tests;
  interp_tests
]

(* This is what gets run *)
let _ = run_test_tt_main suite
