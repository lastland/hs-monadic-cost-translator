module Example where
work n = n+n
expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1
outer :: (Eq a, Num a) => a
outer = let inner :: Num b => b; inner = work 5 in if (inner :: Int) > 0 then expensive 18446744073709551616 else 0
asInt :: Int -> Int
asInt x = outer+x
asInteger :: Integer -> Integer
asInteger x = outer+x
