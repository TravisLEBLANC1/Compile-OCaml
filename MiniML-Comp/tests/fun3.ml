let f (x:int) (y:int) (z:int) = x+y+z in
let g (x:int) (z:int) = f x 2 z in
let h (x:int) = g 30 in
h 10