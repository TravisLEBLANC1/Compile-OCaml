let rec f (a:int) (b:int) :int = if a <= 0 then b else f (a-1) (b+1) in 
f 21 21