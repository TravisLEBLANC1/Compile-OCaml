type tree = E | N of tree * tree

let is_empty (t: tree) = match t with
  | E -> true
  | N(t1, t2) -> false
in
let x = 5 in
x is_empty