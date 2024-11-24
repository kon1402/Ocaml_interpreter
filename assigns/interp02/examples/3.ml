let rec fact = fun n ->
  if n <= 0
  then 1
  else n * fact (n - 1)

  let test : unit = assert (fact 5 = 120)
  