
let group lst =
  let rec helper lst prev acc_group acc_all =
    match lst with
    | [] ->
        begin
          match acc_group with
          | [] -> Some (List.rev acc_all)
          | _ -> Some (List.rev ((List.rev acc_group) :: acc_all))
        end
    | x :: xs ->
        if x = 0 then
          match prev, xs with
          | Some p, y :: _ when y <> 0 && p * y < 0 ->
              (* Zero between nonzero integers of opposite signs *)
              helper xs (Some y) [] ((List.rev acc_group) :: acc_all)
          | _ -> None  (* Invalid zero placement *)
        else
          match prev with
          | None ->
              (* Start a new group with the first nonzero integer *)
              helper xs (Some x) [x] acc_all
          | Some p ->
              if p * x > 0 then
                (* Continue the current group if signs match *)
                helper xs (Some x) (x :: acc_group) acc_all
              else
                (* Invalid: adjacent nonzero integers of different signs *)
                None
  in
  helper lst None [] []
