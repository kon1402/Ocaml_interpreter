open OUnit2
open Utils
open Lib

let _ = run_test_tt_main (
  "all tests" >::: [
    Test_parse.basic_examples;
    Test_interp.suite
  ]
)