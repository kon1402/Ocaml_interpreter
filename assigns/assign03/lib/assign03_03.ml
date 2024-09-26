(* Type definition *)
type tree = 
  | Leaf of int
  | Node of tree list

(* Function to collect terminal elements in the subtree rooted at t *)
let rec terminal_elements t =
  match t with
  | Leaf _ -> [t]
  | Node [] -> [t]
  | Node children ->
      List.concat (List.map terminal_elements children)

(* Function to collect terminal elements in the subtree rooted at t, excluding t itself *)
and terminal_elements_excluding t =
  match t with
  | Leaf _ -> []        (* Exclude the current leaf *)
  | Node [] -> []       (* Exclude the current node if it has no children *)
  | Node children ->
      List.concat (List.map terminal_elements children)

(* Recursive function to collapse the tree *)
let rec collapse_tree h t curr_height =
  match t with
  | Leaf _ -> t  (* Leaves are returned as is *)
  | Node children ->
      if curr_height = h - 1 then
        (* Replace the node's children with terminal elements, excluding the node itself *)
        let terminals = terminal_elements_excluding t in
        Node terminals
      else
        (* Recurse into children, incrementing current height *)
        let new_children = List.map (fun child -> collapse_tree h child (curr_height + 1)) children in
        Node new_children

(* Main collapse function *)
let collapse h t =
  if h <= 0 then
    failwith "Height must be positive"
  else
    collapse_tree h t 0
