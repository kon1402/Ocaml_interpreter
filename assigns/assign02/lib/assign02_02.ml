type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix entries (rows, cols) =
  if rows < 0 || cols < 0 then
    failwith "Number of rows and columns must be non-negative (debugging rip)"
  else if rows = 0 || cols = 0 then
    { entries = []; rows = rows; cols = cols }
  else
    let rec make_rows lst r =
      if r = 0 then []
      else
        let rec take n lst acc =
          match n, lst with
          | 0, _ -> (List.rev acc, lst)
          | _, [] -> failwith "Not enough elements"
          | n, x :: xs -> take (n - 1) xs (x :: acc)
        in
        let (row, rest) = take cols lst [] in
        row :: make_rows rest (r - 1)
    in
    {
      entries = make_rows entries rows;
      rows = rows;
      cols = cols;
    }

(* Test Cases *)
let _ =
  let a = mk_matrix [] (200, 0) in
  let b = { entries = []; rows = 200; cols = 0 } in
  assert (a = b);

  let a = mk_matrix [] (0, 5) in
  let b = { entries = []; rows = 0; cols = 5 } in
  assert (a = b);


  let a = mk_matrix [] (0, 0) in
  let b = { entries = []; rows = 0; cols = 0 } in
  assert (a = b);


  let a = mk_matrix [1.; 2.; 3.; 4.] (2, 2) in
  let b = { entries = [[1.; 2.]; [3.; 4.]]; rows = 2; cols = 2 } in
  assert (a = b);

  try
    let _ = mk_matrix [1.; 2.; 3.] (2, 2) in
    assert false  (* Should not reach here hopefully*)
  with Failure _ ->
    ()  (* Expected exception *)
