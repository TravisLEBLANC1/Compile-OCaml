let sum (a:int) (b:int) = a+b in
let rec mul_internal (c:int) (d:int) (res:int):int =
  if c <= 0 then res else mul_internal (c-1) d (sum res d)
in
mul_internal 6 7 0