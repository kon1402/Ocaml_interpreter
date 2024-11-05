open OUnit2
open Utils
open Lib  (* Adjust the module name if necessary *)

(* Custom equality functions *)
let rec value_equal v1 v2 =
  match v1, v2 with
  | VNum n1, VNum n2 -> n1 = n2
  | VBool b1, VBool b2 -> b1 = b2
  | VUnit, VUnit -> true
  | VFun _, VFun _ -> true  (* Treat all functions as equal *)
  | _, _ -> false

let result_equal r1 r2 =
  match r1, r2 with
  | Ok v1, Ok v2 -> value_equal v1 v2
  | Error e1, Error e2 -> e1 = e2
  | _, _ -> false

(* Test function *)
let test s e =
  let description = "Testing interp \"" ^ s ^ "\"" in
  let test_fun _ =
    let result = Lib.interp s in
    assert_bool description (result_equal result e)
  in
  description >:: test_fun

(* Test suite *)
let basic_examples = "Basic interpreter examples" >:::
[ test "2 + 3" (Ok (VNum 5))
; test "true && false" (Ok (VBool false))
; test "let x = 5 in x" (Ok (VNum 5))
; test "if true then 1 else 0" (Ok (VNum 1))
; test "fun x -> x + 1" (Ok (VFun ("x", Bop (Add, Var "x", Num 1))))
; test "(fun x -> x + 1) 5" (Ok (VNum 6))
; test "let f = fun x -> x * x in f 3" (Ok (VNum 9))
; test "x + 1" (Error (UnknownVar "x"))
; test "10 / 0" (Error DivByZero)
; test "let x = 5 in x y" (Error InvalidApp)
; test "if true then false else" (Error ParseFail)
; test "let x = 1 in" (Error ParseFail)
; test "fun x ->" (Error ParseFail)
; (* Add more test cases as needed *)
]

(* Run the test suite *)
let () = OUnit2.run_test_tt_main basic_examples