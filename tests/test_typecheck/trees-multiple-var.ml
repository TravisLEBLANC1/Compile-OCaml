
type tree = E | N of tree * tree

let is_empty (t: tree) = match t with
  | E -> true
  | N(t1, t2) -> false
in
let is_singleton (t: tree) = match t with
  | E -> false
  | N(E, E) -> true
  | N(t1, t2) -> false
in
let rec size (t: tree): int = match t with
  | E -> 1
  | N(E, E) -> 3
  | N(N(t1, t2),N(t3, t1)) -> 3 + (size t1) + (size t2) +(size t3) + (size t1) (*multiple variables same name*)
  | N(t1, t2) -> 1 + (size t1) + (size t2)
in

let max (x: int) (y: int) = if x < y then y else x in
let rec height (t: tree): int = match t with
  | E -> 0
  | N(t1, t2) -> 1 + max (height t1) (height t2)
in

let t = N(N(E,E), N(N(E,E), N(E,E))) in size t (*expect 11*)
