module Example where
matchFirst n (x:_) | n <= 0 = 0 | otherwise = x
guardFirst n _ | n <= 0 = 0
guardFirst _ (x:_) = x
demanded x = matchFirst (0::Int) undefined + x
unused x = guardFirst (0::Int) undefined + x
unmatched x = case ([] :: [Int]) of { y:_ -> x+y }
