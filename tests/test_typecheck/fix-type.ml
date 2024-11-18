type tree = E | N of tree * tree

let is_empty (t: tree) = match t with
  | E -> true
  | N(t1, t2) -> false
in

let rec f (t:tree) :tree = match t with
  | E -> true
  | N(t1, t2) -> is_empty (f t1)
in
let x = E in 
f x