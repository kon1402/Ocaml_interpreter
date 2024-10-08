
(* Type definition *)
type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(* Tail-recursive sum function using CPS *)
let sum_tr total =
  let rec go total cont =
    match total with
    | Leaf -> cont 0
    | Node (x, left, right) ->
        (* Define a continuation for the left subtree *)
        let cont_l sum_left =
          (* Define a continuation for the right subtree *)
          let cont_r sum_right =
            (* Continue with the sum of x, sum_l, and sum_r *)
            cont (x + sum_left + sum_right)
          in
          (* Recursively process the right subtree *)
          go right cont_r
        in
        (* Recursively process the left subtree *)
        go left cont_l
  in
  (* Start the recursion with the identity continuation *)
  go total (fun x -> x)
