module Example where
f x = let g :: Monoid a => a -> a; g y = y in x
