module Example where
empty n = []
ints :: [Int] -> Int
ints [] = 5
bools :: [Bool] -> Int
bools [] = 5
local x = let shared = empty x in ints shared + bools shared
