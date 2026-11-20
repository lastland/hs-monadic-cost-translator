module Example where

data Box = Box Int

keep :: Int -> Int
keep x = x

partial :: [Int] -> [Int]
partial xs = map keep xs
