module Example where
mutual x = let evenN n | n == 0 = x | otherwise = oddN (n-1); oddN n | n == 0 = 0 | otherwise = evenN (n-1) in evenN 4
