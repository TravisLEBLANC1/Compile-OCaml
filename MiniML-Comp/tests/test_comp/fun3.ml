let p = 6 in 
let f (x:int) (y:int) (z:int) = x+y+z+p in
let g (z:int) = f 24 2 z in
g 10