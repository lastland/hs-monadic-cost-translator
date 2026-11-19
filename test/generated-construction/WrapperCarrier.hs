module WrapperCarrier where

alternative :: (Int -> [Int]) -> [Int] -> [Int]
alternative k xs = k =<< xs

generic :: Functor f => f Int -> f Int
generic xs = id <$> xs

unused :: Int
unused = let hidden = id <$> [True] in 3
