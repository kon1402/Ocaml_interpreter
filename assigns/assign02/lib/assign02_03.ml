
type dir = 
  | North
  | South
  | East
  | West
type path = dir list

let dist dirs =
  let rec sum_dirs dirs (x, y) =
    match dirs with
    | [] -> (x, y)
    | dir :: rest ->
      let (dx, dy) = match dir with
        | North -> (0., 1.)
        | South -> (0., -1.)
        | East  -> (1., 0.)
        | West  -> (-1., 0.)
      in
      sum_dirs rest (x +. dx, y +. dy)
  in
  let (x, y) = sum_dirs dirs (0., 0.) in
  sqrt (x *. x +. y *. y)
