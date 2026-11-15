module Example where
expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1
shared :: (Eq a, Num a) => a
shared = expensive 18446744073709551616
generic x = shared+x
asInt :: Int -> Int
asInt x = generic x
asInteger :: Integer -> Integer
asInteger x = generic x
