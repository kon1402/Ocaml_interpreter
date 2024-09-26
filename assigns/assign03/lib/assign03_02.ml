
let gen_fib l k =
  (* Helper function to get the nth element of a list *)
  let rec get_nth lst n =
    match lst with
    | [] -> failwith "Index out of bounds"
    | x :: xs -> if n = 0 then x else get_nth xs (n - 1)
  in
  (* Helper function to sum all elements of a list *)
  let rec sum lst =
    match lst with
    | [] -> 0
    | x :: xs -> x + sum xs
  in
  (* Helper function to remove the last element of a list and return the remaining list and the last element *)
  let rec remove_last lst acc =
    match lst with
    | [] -> failwith "Unexpected empty list"
    | [x] -> (List.rev acc, x)
    | x :: xs -> remove_last xs (x :: acc)
  in
  if k < 0 || l = [] then
    failwith "Undefined behavior: empty list or negative index"
  else
    let len_l = List.length l in
    if k < len_l then
      get_nth l k
    else
      let last_terms = List.rev l in  (* Reverse to have the most recent term at the head *)
      let sum_last_terms = sum last_terms in
      let rec aux n last_terms sum_last_terms len_last_terms =
        if n > k then
          match last_terms with
          | [] -> failwith "Unexpected empty last_terms"
          | x :: _ -> x
        else
          let new_value = sum_last_terms in
          let sum_last_terms' = sum_last_terms + new_value in
          let last_terms' = new_value :: last_terms in
          let len_last_terms' = len_last_terms + 1 in
          if len_last_terms' > len_l then
            let (last_terms'', oldest_value) = remove_last last_terms' [] in
            let sum_last_terms'' = sum_last_terms' - oldest_value in
            aux (n + 1) last_terms'' sum_last_terms'' len_l
          else
            aux (n + 1) last_terms' sum_last_terms' len_last_terms'
      in
      aux len_l last_terms sum_last_terms len_l
