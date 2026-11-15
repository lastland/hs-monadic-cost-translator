module Example where
data Box a = Box a Int
wrap x = Box x undefined
unbox (Box x _) = x
boxed x = unbox (wrap x)
