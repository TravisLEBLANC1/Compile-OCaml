let f (x:int) (y:int) (z:int) = x+y+z in
let g (x:int) = f x in  
g 1 1 0 + f 20 1 1 + f 3 3 3 + f 3 3 3