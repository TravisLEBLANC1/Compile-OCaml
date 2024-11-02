let rec sum (a:int) (b:int) :int = if a <= 0 then b else sum (a-1) (b+1) in 
let mul (a:int) (b:int) = 
  let rec mul_internal (c:int) (d:int) (res:int):int =
    if c <= 0 then res else mul_internal (c-1) d (sum res d)
  in
  mul_internal a b 0
in
mul 6 7