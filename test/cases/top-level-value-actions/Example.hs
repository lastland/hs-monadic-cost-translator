module Example where

shared :: Int
shared = 10

bad :: Int
bad = error "demanded global failure"

f, g, test :: () -> Int
f _ = shared
g _ = shared
test _ = f () + g ()
