type tree = E | N of tree * tree

let max (x: int) (y: int) = if x < y then y else x in
let rec height (t: tree): int = match t with
  | E -> 0
  | N(t1, t2) -> 1 + max (height t1) (height t2)
in

let t = N(E,N(E,E)) in

if height t > 5 then
  2
else
  42