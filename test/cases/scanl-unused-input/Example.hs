module Example where
add a b = a + b
bad n = if (undefined :: Bool) then [n] else []
scanned x = head (scanl add x (bad x))
