
let rec pow k n = if n = 0 then 1 else k * pow k (n-1)
let is_prime x = 
  let rec check n = if n * n > x then true else if x mod n = 0 then false else check (n + 1) in 
  if x < 2 then false
  else check 2
  let rec nth_prime n =
    match n with
    | 0 -> 2
    | 1 -> 3
    | n -> let rec findprime x = if is_prime x then x else findprime (x + 2) in findprime (nth_prime (n - 1) + 2)

  let rec power_of_prime n p =
      if n mod p = 0 then 1 + power_of_prime (n / p) p
      else 0
  let nth s i =
      let rec extract_nth s i idx =
        let prime = nth_prime idx in
        let exponent = power_of_prime s prime in
        let new_s = s / (pow prime exponent) in
        if idx = i then exponent
        else extract_nth new_s i (idx + 1)
      in
      extract_nth s i 0