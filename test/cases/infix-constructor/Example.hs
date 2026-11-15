module Example where
data Pair a = a :*: a
first (x :*: _) = x
paired x = first (x :*: undefined)
