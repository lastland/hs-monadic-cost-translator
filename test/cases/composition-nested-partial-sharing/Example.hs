module Example where
add a b = a+b
twice x = x+x
observed x = foldr add 0 (map (((add . twice) . twice) x) [x,x])
