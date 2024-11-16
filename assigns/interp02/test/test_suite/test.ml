open OUnit2
open Utils
open Stdlib320

(* Custom equality functions *)
let value_equal v1 v2 =
  match v1, v2 with
  | VNum n1, VNum n2 -> n1 = n2
  | VBool b1, VBool b2 -> b1 = b2
  | VUnit, VUnit -> true
  | VClosure _, VClosure _ -> true 
  | _, _ -> false

let result_equal r1 r2 =
  match r1, r2 with
  | Ok v1, Ok v2 -> value_equal v1 v2
  | Error e1, Error e2 -> e1 = e2
  | _, _ -> false

(* Test helper functions *)
let test_parse s expected =
  let description = "Testing parse \"" ^ s ^ "\"" in
  let test_fun _ =
    let result = Lib.parse s in
    assert_equal expected result
  in
  description >:: test_fun

let test_type_of e expected =
  let description = "Testing type_of" in
  let test_fun _ =
    let result = Lib.type_of e in
    assert_equal expected result ~printer:(function
      | Ok ty -> string_of_ty ty
      | Error e -> string_of_error e)
  in
  description >:: test_fun

let test_eval e expected =
  let description = "Testing eval" in
  let test_fun _ =
    let result = Lib.eval e in
    assert_equal expected result ~printer:string_of_value
  in
  description >:: test_fun

let test_interp s expected =
  let description = "Testing interp \"" ^ s ^ "\"" in
  let test_fun _ =
    let result = Lib.interp s in
    assert_bool description (result_equal result expected)
  in
  description >:: test_fun

(* Test suites *)
let parse_tests = "Parse tests" >:::
[ test_parse "let x : int = 5 in x" (Some [DLet ("x", [], TInt, Num 5)])
; test_parse "let f (x : int) : int = x + 1" 
    (Some [DLet ("f", [("x", TInt)], TInt, Bop(Add, Var "x", Num 1))])
; test_parse "let rec fact (n : int) : int = if n <= 0 then 1 else n * fact (n - 1)"
    (Some [DLetRec ("fact", ("n", TInt), [], TInt, 
           If(Bop(Lte, Var "n", Num 0), 
              Num 1, 
              Bop(Mul, Var "n", App(Var "fact", Bop(Sub, Var "n", Num 1)))))])
]

let type_of_tests = "Type checking tests" >:::
[ test_type_of (Num 5) (Ok TInt)
; test_type_of (Bop(Add, Num 1, Num 2)) (Ok TInt)
; test_type_of (If(True, Num 1, Num 2)) (Ok TInt)
; test_type_of (If(Num 1, True, False)) (Error (IfCondTyErr TInt))
; test_type_of (Fun("x", TInt, Var "x")) (Ok (TArrow(TInt, TInt)))
; test_type_of (App(Num 1, Num 2)) (Error (FunAppTyErr TInt))
]

let eval_tests = "Evaluation tests" >:::
[ test_eval (Num 5) (VNum 5)
; test_eval Unit VUnit
; test_eval (Fun("x", TInt, Var "x")) (VClosure("x", Var "x", []))
; test_eval (If(True, Num 1, Num 2)) (VNum 1)
; test_eval (Bop(Add, Num 2, Num 3)) (VNum 5)
]

let interp_tests = "Integration tests" >:::
[ test_interp "let x : int = 5 in x" (Ok (VNum 5))
; test_interp "let f (x : int) : int = x + 1 in f 5" (Ok (VNum 6))
; test_interp "let rec fact (n : int) : int = 
               if n <= 0 then 1 
               else n * fact (n - 1) 
               in fact 5" (Ok (VNum 120))
; test_interp "1 + true" (Error (OpTyErrR Add TInt TBool))
; test_interp "let x = 1" (Error ParseFail)
]

(* Run all tests *)
let suite = "All tests" >::: 
[ parse_tests
; type_of_tests
; eval_tests
; interp_tests
]

let () = run_test_tt_main suite