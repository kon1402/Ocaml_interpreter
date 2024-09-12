
let rec pow k n = if n = 0 then 1 else k * pow k (n-1)
  