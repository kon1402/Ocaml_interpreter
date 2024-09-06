
let sqrt n =
  let rec find_sqrt x = if x * x >= n then x else find_sqrt (x + 1) in
  find_sqrt 0