module Example where

ignore :: () -> Int
ignore _ = 7

apply :: Int -> Int
apply n = ignore () + n
