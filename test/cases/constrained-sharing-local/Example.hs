module Example where
expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1
local x = let shared :: (Eq a, Num a) => a; shared = expensive 18446744073709551616; one n = (shared :: Int)+n; two n = (shared :: Integer)+n in if one 0 == 1 && two 0 == 2 then x else 0
