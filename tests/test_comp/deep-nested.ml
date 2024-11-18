
type tree = E | N of tree * tree

let rec f (t:tree):int = match t with
  | N(N(N(N(N(N(N(E,t7),t6),t5),t4),t3),t2),t1) -> 2048
  | E -> 21
  | N(t1,t2) -> f(t1) + f(t2)
in
let t = N(E,E) in
f t