let f (a:int) (b:int) =
  let mul (a:int) (b:int) = a*b in
  mul (a+b) (a-b)
in
2 + f 7 3