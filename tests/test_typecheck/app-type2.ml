type tree = E | N of tree * tree

let is_empty (t: tree) = match t with
  | E -> true
  | N(t1, t2) -> false
in
let x = E in
let y = E in 
is_empty x y