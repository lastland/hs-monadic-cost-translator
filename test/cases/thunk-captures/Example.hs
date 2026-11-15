module Example where
pair a b = (a,b)
keep x = x
partial x xs = map (pair x) xs
composed x xs = map (pair x . keep) xs
