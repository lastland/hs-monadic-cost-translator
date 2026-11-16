module Example where

unused :: Int
unused = error "unused top-level value was evaluated"

plain :: Int -> (Int, Int)
plain x = (x, x)
