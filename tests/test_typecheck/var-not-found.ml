type tree = E | N of tree*tree

let f (t:tree) = match t with
  | E -> 1
in 
f x