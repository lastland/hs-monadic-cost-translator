module Example where

unused :: Int
unused = forward (error "unused top-level value was evaluated")

work :: Int -> Int
work n = n + n

forward :: Int -> Int
forward n = work n + 1

plain :: Int -> (Int, Int)
plain n = (forward n, forward n)
