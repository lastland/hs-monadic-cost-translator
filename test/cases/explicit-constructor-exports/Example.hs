module Example (Box(..), make, readBox, observed) where
data Box = Box Int
make x = Box x
readBox (Box x) = x
observed x = readBox (make x)
