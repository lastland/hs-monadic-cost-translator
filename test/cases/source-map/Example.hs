module Example where

data Box = Box { payload :: Int }

keep :: Int -> Int
keep x = x

partial :: [Int] -> [Int]
partial xs = map keep xs

zero :: Int
zero = 0
