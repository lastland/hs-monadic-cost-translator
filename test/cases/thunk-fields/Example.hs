module Example where
data Box a = Box { first :: a, second :: a }
constructor x = Box x x
record x = Box { second = (x), first = x }
tuple x = (x,x)
list x = [x,x]
cons x xs = x : xs
bad :: a -> a
bad x = error "computed field evaluated"
mixed x = Box x (bad x)
