type tree = E | N of tree*tree

let f (t:tree) = match t with
  | E -> 0
  | N(t1,t2,t3) -> 1
in 
let x = E in
f x