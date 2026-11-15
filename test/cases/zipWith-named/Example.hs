module Example where
add a b = a + b
zipped x = head (zipWith add (x:undefined) (x:undefined))
