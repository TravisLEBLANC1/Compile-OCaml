let p = 6 in 
let f (x:int) (y:int) (z:int) = x+y+z in
let g (z:int) = p + f 24 2 z in
g 10