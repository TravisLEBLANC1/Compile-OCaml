let f (x:int) (y:int) (z:int) = x+y+z in
let g (z:int) = f 30 2 z in
let h = f 30 in
h 10 2