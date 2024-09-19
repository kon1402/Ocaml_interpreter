
(* Type Definitions *)
type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index


let get_pos (board : board) ((row_idx, col_idx) : pos_index) : pos =
  let (row1, row2, row3) = board in
  let row = match row_idx with
    | Top -> row1
    | Middle -> row2
    | Bottom -> row3
  in
  let (col1, col2, col3) = row in
  match col_idx with
    | Left -> col1
    | Middle -> col2
    | Right -> col3


let winner (board : board) : bool =
  let (row1, row2, row3) = board in
  let (a1, a2, a3) = row1 in
  let (b1, b2, b3) = row2 in
  let (c1, c2, c3) = row3 in

  let check_line p1 p2 p3 =
    match (p1, p2, p3) with
    | (Piece s1, Piece s2, Piece s3) when s1 = s2 && s2 = s3 -> true
    | _ -> false
  in

  (* Check all possible winning lines *)
  (* Rows *)
  check_line a1 a2 a3 ||
  check_line b1 b2 b3 ||
  check_line c1 c2 c3 ||

  (* Columns *)
  check_line a1 b1 c1 ||
  check_line a2 b2 c2 ||
  check_line a3 b3 c3 ||

  (* Diagonals *)
  check_line a1 b2 c3 ||
  check_line a3 b2 c1
