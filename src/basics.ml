let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =
  match tup with
  | (a, b) -> (b, a)

let rev_triple tup =
  match tup with
  | (a, b, c) -> (c, b, a)

let is_odd x =
  x mod 2 <> 0

let is_older date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 <> y2 then y1 < y2
  else if m1 <> m2 then m1 < m2
  else d1 < d2

let to_us_format date1 =
  let (y, m, d) = date1 in
  (m, d, y)

(************************)
(* Part 2: Recursion    *)
(************************)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth (idx, lst) =
  match (idx, lst) with
  | (0, x :: _) -> x
  | (k, _ :: xs) when k > 0 -> get_nth (k - 1, xs)
  | _ -> failwith "get_nth: index out of bounds"

let larger lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 > len2 then lst1
  else if len2 > len1 then lst2
  else []

let sum lst1 lst2 =
  List.fold_left ( + ) 0 lst1 + List.fold_left ( + ) 0 lst2
