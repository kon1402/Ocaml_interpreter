
let is_prime x = 
  let rec check n = if n * n > x then true else if x mod n = 0 then false else check (n + 1) in 
  if x < 2 then false
  else check 2
  let rec nth_prime n =
    match n with
    | 0 -> 2
    | 1 -> 3
    | n -> let rec findprime x = if is_prime x then x else findprime (x + 2) in findprime (nth_prime (n - 1) + 2)