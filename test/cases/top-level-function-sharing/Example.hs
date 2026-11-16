module Example where

work :: Int -> Int
work x = x + x

shared :: Int
shared = work 5

f, g :: () -> Int
f _ = shared
g _ = shared

test :: () -> Int
test _ = f () + g ()
