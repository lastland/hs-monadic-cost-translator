module Example where
work x = x + x
add a b = a + b
shared x = let y = work x in add y y
