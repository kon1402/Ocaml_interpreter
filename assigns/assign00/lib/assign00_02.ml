open Assign00_01
let is_prime x = 
  let rec check n = if n * n > x then true else if x mod n = 0 then false else check (n + 1) in 
  if x < 2 then false
  else check 2
