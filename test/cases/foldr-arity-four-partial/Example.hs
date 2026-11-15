module Example where
add4 a b c d = a + b + c + d
folded x = foldr (add4 x x) 0 [x,x]
