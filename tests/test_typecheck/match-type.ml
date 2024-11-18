type tree = E | N of tree * tree

let is_empty (t: tree) = match t with
  | E -> true
  | N(t1, t2) -> 42
in
is_empty (E)