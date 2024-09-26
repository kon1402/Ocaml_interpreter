
type tree = 
  | Leaf of int
  | Node of tree list

(* Function to collect terminal elements in a tree *)
let rec terminal_elements t =
  match t with
  | Leaf _ -> [t]
  | Node [] -> [t]  (* Node with no children is terminal *)
  | Node children ->
      List.concat (List.map terminal_elements children)

(* Recursive function to collapse the tree *)
let rec collapse_tree h t curr_height =
  match t with
  | Leaf _ -> t  (* Leaves are returned as is *)
  | Node children ->
      if curr_height = h - 1 then
        (* Replace the node's children with terminal elements *)
        let terminals = terminal_elements t in
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
