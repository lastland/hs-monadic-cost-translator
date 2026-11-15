{-# LANGUAGE ScopedTypeVariables #-}
module Example where
work n | n == 0 = n+1 | otherwise = work (n-n)+1
shared :: (Num a, Eq a) => a
shared = work 18446744073709551616
observed :: forall a. (Num a, Eq a) => a -> Int
observed x = case (shared :: a) of y -> let inner :: forall a. a -> Int; inner z = if y == 1 then 1 else 2 in inner True
