
(* Custom function to find the value associated with a key *)
let rec find_assoc key lst =
  match lst with
  | [] -> None
  | (k, v) :: rest ->
    if k = key then Some v
    else find_assoc key rest

(* Custom function to remove all occurrences of a key from the list *)
let rec remove_key key lst =
  match lst with
  | [] -> []
  | (k, v) :: rest ->
    if k = key then remove_key key rest
    else (k, v) :: remove_key key rest

(* Main function *)
let mk_unique_keys alst =
  let rec aux input acc =
    match input with
    | [] -> acc
    | (key, value) :: rest ->
      let existing_value = find_assoc key acc in
      let acc_without_key = remove_key key acc in
      let new_value = match existing_value with
        | None -> value
        | Some v -> v + value
      in
      aux rest ((key, new_value) :: acc_without_key)
  in
  aux alst []
