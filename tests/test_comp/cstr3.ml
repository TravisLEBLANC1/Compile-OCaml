type test = N of test*test | E

let deap_nested (t:test) = match t with
  | N(N(N(N(N(E,E), E), E), E), E) ->  0
  | E -> 42
  | N(t1, t2) -> 2
in 
let t = E in
deap_nested t