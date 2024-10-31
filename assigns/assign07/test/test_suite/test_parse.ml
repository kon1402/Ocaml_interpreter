open OUnit2
open Utils

let test s e =
  let d = "testing parse \"" ^ s ^ "\"" in
  let t _ =
    let a = My_parser.parse s in
    assert_equal e a
  in d >: test_case ~length:(Custom_length 2.) t

  let basic_examples = "basic parse examples" >:::
  [ test "2 + 3" (Some (Bop (Add, Num 2, Num 3)))
  ; test "" None
  ; test "3 + 3 * 3 + 3" (Some (Bop (Add, Bop (Add, Num 3, Bop (Mul, Num 3, Num 3)), Num 3)))
  ; test "(fun x -> x + 1) 2 3 4" (Some (App (App (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 2), Num 3), Num 4)))
  ; test "let x = 2 in x" (Some (Let ("x", Num 2, Var "x")))
  ; test "let x = x in in" None
  ; test "let fun = 2 in x" None
  ; test "let f = (fun x -> x) in f ()" (Some (Let ("f", Fun ("x", Var "x"), App (Var "f", Unit))))
  ; test "2 mod (if true then 2 else 3)" (Some (Bop (Mod, Num 2, If (True, Num 2, Num 3))))
  ; test "(((( 2 <= 3 <= 4 ))))" (Some (Bop (Lte, Bop (Lte, Num 2, Num 3), Num 4)))
  
    (* Additional easy test cases *)
  ; test "true" (Some True)
  ; test "false" (Some False)
  ; test "x" (Some (Var "x"))
  ; test "()" (Some Unit)
  ; test "if true then 1 else 0" (Some (If (True, Num 1, Num 0)))
  ; test "let y = 5 in y" (Some (Let ("y", Num 5, Var "y")))
  ; test "fun x -> x" (Some (Fun ("x", Var "x")))
  ; test "f x" (Some (App (Var "f", Var "x")))
  ; test "1 = 2" (Some (Bop (Eq, Num 1, Num 2)))
  ; test "3 <> 4" (Some (Bop (Neq, Num 3, Num 4)))
  ; test "x && y" (Some (Bop (And, Var "x", Var "y")))
  ; test "a || b" (Some (Bop (Or, Var "a", Var "b")))
  ; test "5 < 10" (Some (Bop (Lt, Num 5, Num 10)))
  ; test "15 > 20" (Some (Bop (Gt, Num 15, Num 20)))
  ; test "if x then y else z" (Some (If (Var "x", Var "y", Var "z")))
  ; test "let z = 3 in z + 1" (Some (Let ("z", Num 3, Bop (Add, Var "z", Num 1))))
  ; test "(1 + 2) * 3" (Some (Bop (Mul, Bop (Add, Num 1, Num 2), Num 3)))
  ; test "fun x -> x + y" (Some (Fun ("x", Bop (Add, Var "x", Var "y"))))
  ; test "x y" (Some (App (Var "x", Var "y")))
  ; test "(x y) z" (Some (App (App (Var "x", Var "y"), Var "z")))
  ; test "let f = fun x -> x in f 10" (Some (Let ("f", Fun ("x", Var "x"), App (Var "f", Num 10))))
  ; test "if false then true else false" (Some (If (False, True, False)))
  ; test "let a = 1 in let b = 2 in a + b" (Some (Let ("a", Num 1, Let ("b", Num 2, Bop (Add, Var "a", Var "b")))))
  ; test "fun x -> x * x" (Some (Fun ("x", Bop (Mul, Var "x", Var "x"))))
  ; test "x + y - z" (Some (Bop (Sub, Bop (Add, Var "x", Var "y"), Var "z")))
  ; test "if true then x else y + z" (Some (If (True, Var "x", Bop (Add, Var "y", Var "z"))))
  ; test "(fun x -> x + 1) (2 * 3)" (Some (App (Fun ("x", Bop (Add, Var "x", Num 1)), Bop (Mul, Num 2, Num 3))))
  ; test "let x = 10 in fun y -> x + y" (Some (Let ("x", Num 10, Fun ("y", Bop (Add, Var "x", Var "y")))))
  ; test "if true then if false then 1 else 2 else 3" (Some (If (True, If (False, Num 1, Num 2), Num 3)))
  ; test "x + (y * z)" (Some (Bop (Add, Var "x", Bop (Mul, Var "y", Var "z"))))
  ; test "(x + y) * z" (Some (Bop (Mul, Bop (Add, Var "x", Var "y"), Var "z")))
  ; test "fun x -> fun y -> x && y" (Some (Fun ("x", Fun ("y", Bop (And, Var "x", Var "y")))))
  ; test "if (x = y) then z else w" (Some (If (Bop (Eq, Var "x", Var "y"), Var "z", Var "w")))
  ; test "let x = 5 in x x" (Some (Let ("x", Num 5, App (Var "x", Var "x"))))
  ; test "fun x -> x x" (Some (Fun ("x", App (Var "x", Var "x"))))
  ; test "if true then false else" None
  ; test "let x = 1 in" None
  ; test "fun x ->" None
  ]

