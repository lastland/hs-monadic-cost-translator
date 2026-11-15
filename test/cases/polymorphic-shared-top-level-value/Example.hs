module Example where
empty n | n > 0 = [] | otherwise = []
shared = empty (5::Int)
ints x = case shared of { [] -> x; _ -> x }
bools x = case shared of { [] -> x; _ -> x }
