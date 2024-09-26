
let gen_fib l k =
  let len_l = List.length l in
  (* Helper function to compute the sum of the last 'len_l' terms *)
  let sum_last_terms current_list len_l =
    let rec aux_sum i acc =
      if i >= len_l then acc
      else aux_sum (i + 1) (acc + List.nth current_list i)
    in
    aux_sum 0 0
  in
  (* Tail-recursive helper function *)
  let rec aux n prev_values =
    if n < len_l then List.nth l n (* Use the initial values from the list l *)
    else if n = k then List.nth prev_values 0 (* Base case: return the nth value when we reach k *)
    else
      let new_value = sum_last_terms (List.rev prev_values) len_l in
      aux (n + 1) (new_value :: prev_values) (* Recurse and build the list *)
  in
  aux 0 (List.rev l)

