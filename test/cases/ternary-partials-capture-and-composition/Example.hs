module Example where
add3 a b c = a + b + c
twice x = x + x
partial x = let captured z = x + z in head (map (captured . add3 x x . twice) (x : undefined))
