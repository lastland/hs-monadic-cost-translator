module Example where
add4 a b c d = a+b+c+d
twice x = x+x
observed x = foldr ((add4 . twice) x x) 0 [x,x]
