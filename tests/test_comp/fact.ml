let rec fact (n:int) :int= 
  if n <= 1
  then 1
  else n * fact (n-1)
in
fact 6