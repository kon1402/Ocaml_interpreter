(* Helper function to do k^n *)
let rec pow k n = 
  if n = 0 then 1 
  else k * pow k (n-1)

(* Function checking if prime*)
let is_prime x = 
  let rec check n = if n * n > x then true else if x mod n = 0 then false else check (n + 1) in if x < 2 then false
  else check 2

(* Function 2 *)
let rec nth_prime n =
  match n with
  | 0 -> 2
  | 1 -> 3
  | n -> 
      let rec findprime x = if is_prime x then x else findprime (x + 2) in findprime (nth_prime (n - 1) + 2)

(* Helper function *)
let rec power_of_prime n p =
  if n mod p = 0 then 1 + power_of_prime (n / p) p
  else 0

(* Function 3 *)
let decode_sequence s =
  let rec extract s idx =
    let prime = nth_prime idx in
    let exponent = power_of_prime s prime in if s = 1 then [] (* If remaining s is 1, stop *)
    else if exponent = 0 then extract s (idx + 1) else exponent :: extract (s / (pow prime exponent)) (idx + 1) in extract s 0

(*using the above listed helper
functions for to_string *)
let to_string s =
  let seq = decode_sequence s in
  let rec format_list = function
    | [] -> ""
    | [x] -> string_of_int x
    | x::xs -> string_of_int x ^ "; " ^ format_list xs in "[" ^ format_list seq ^ "]"

