module Example where
add a b = a + b
folded x = foldl add 0 [x,x]
