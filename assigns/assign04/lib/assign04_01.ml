
let last_function_standing funcs start pred =
  (* Helper function to compute the lifespan of a function f *)
  let rec lifespan f s seen n =
    if pred s then Some n  (* Lifespan is n when pred(s) is true *)
    else if List.mem s seen then None  (* Cycle detected: infinite lifespan *)
    else lifespan f (f s) (s :: seen) (n + 1)
  in
  (* Collect lifespans for each function *)
  let rec collect_lifespans funcs acc =
    match funcs with
    | [] -> acc
    | f :: rest ->
        let l = lifespan f start [] 0 in
        collect_lifespans rest ((l, f) :: acc)
  in
  let lifespans = collect_lifespans funcs [] in
  (* Determine the maximal lifespan *)
  let max_lifespan =
    List.fold_left
      (fun acc (l, _) ->
        match acc, l with
        | None, _ -> None  (* Infinite lifespan is maximal *)
        | _, None -> None  (* Infinite lifespan found *)
        | Some m, Some n -> Some (if n > m then n else m))
      (Some (-1)) lifespans
  in
  (* Collect functions with maximal lifespan *)
  let max_funcs =
    List.fold_left
      (fun acc (l, f) ->
        if l = max_lifespan then f :: acc else acc)
      [] lifespans
  in
  match max_funcs with
  | [f] -> Some f
  | _ -> None
