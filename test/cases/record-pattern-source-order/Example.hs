module Example where
data R = R { a :: Int, b :: Int }
f R { b=0, a=0 } = 1
f _ = 2
observed x = f (R undefined 1)
