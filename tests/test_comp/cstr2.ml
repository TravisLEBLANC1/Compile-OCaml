type test = N of test*test | E | B | A of test

let rec nb_nodes (t:test):int = match t with 
  | E -> 0
  | B -> 0
  | A(B) -> 1
  | A(A(t1)) -> nb_nodes t1 
  | A(N(t1,t2)) -> 2 + nb_nodes t1 + nb_nodes t2
  | A(E) -> 40
  | N(t1, t2) -> 1 + nb_nodes t1 + nb_nodes t2

in

let t = A(A(A(N(E, A(E))))) in

nb_nodes t