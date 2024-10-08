
(* Definition of set_info *)
type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

(* Module ListSet *)
module ListSet = struct

  type t = int list  (* Sets are represented as sorted lists without duplicates *)

  (* Check if an element is a member of the set *)
  let rec mem x s =
    match s with
    | [] -> false
    | y :: ys ->
        if y = x then true
        else if y > x then false  (* Since the list is sorted *)
        else mem x ys

  (* The empty set *)
  let empty = []

  (* Create a singleton set *)
  let singleton x = [x]

  (* Cardinality of the set *)
  let card s = List.length s

  (* Union of two sets *)
  let rec union s1 s2 =
    match s1, s2 with
    | [], s -> s
    | s, [] -> s
    | x1 :: xs1, x2 :: xs2 ->
        if x1 = x2 then
          x1 :: union xs1 xs2
        else if x1 < x2 then
          x1 :: union xs1 s2
        else
          x2 :: union s1 xs2
end

(* Module FuncSet *)
module FuncSet = struct

  type t = set_info  (* Sets are represented using the set_info record *)

  (* Check if an element is a member of the set *)
  let mem x t = t.ind x

  (* The empty set *)
  let empty = {
    ind = (fun _ -> false);  (* Indicator function returns false for all elements *)
    mn = 1;  (* mn > mx indicates an empty set *)
    mx = 0;
  }

  (* Create a singleton set *)
  let singleton x = {
    ind = (fun y -> y = x);  (* Indicator function returns true only for x *)
    mn = x;
    mx = x;
  }

  (* Cardinality of the set *)
  let card t =
    if t.mn > t.mx then 0  (* Empty set *)
    else
      let rec count x acc =
        if x > t.mx then acc
        else if t.ind x then count (x + 1) (acc + 1)
        else count (x + 1) acc
      in
      count t.mn 0

  (* Union of two sets *)
  let union t1 t2 =
    let ind x = t1.ind x || t2.ind x in  (* Indicator function for the union *)
    if t1.mn > t1.mx then t2  (* t1 is empty *)
    else if t2.mn > t2.mx then t1  (* t2 is empty *)
    else
      let min_candidate = min t1.mn t2.mn in
      let max_candidate = max t1.mx t2.mx in

      (* Find the minimum element in the union *)
      let rec find_mn x =
        if x > max_candidate then None
        else if ind x then Some x
        else find_mn (x + 1)
      in

      (* Find the maximum element in the union *)
      let rec find_mx x =
        if x < min_candidate then None
        else if ind x then Some x
        else find_mx (x - 1)
      in

      match find_mn min_candidate, find_mx max_candidate with
      | Some mn, Some mx ->
          { ind = ind; mn = mn; mx = mx }
      | _ ->
          { ind = ind; mn = 1; mx = 0 }  (* Empty set *)
end
