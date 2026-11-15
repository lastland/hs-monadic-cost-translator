module Example where
expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1
shared :: (Eq a, Num a) => (a, [b])
shared = (expensive 18446744073709551616, [])
one x = case (shared :: (Int,[Bool])) of { (n,_) -> n+x }
two x = case (shared :: (Int,[Char])) of { (n,_) -> n+x }
