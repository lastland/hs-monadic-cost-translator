module Example where
data Pair = Pair { first :: Int, second :: Int }
unbox :: Pair -> Int
unbox Pair { first = x, second = _ } = x
captured :: Int -> Int
captured x = let y = x+x; add z = y+z in add x+add x
choose :: Int -> Int
choose 0 = 0
choose x = x
composed :: Int -> Int
composed partial = (captured . captured) partial
singleton :: Int -> [Int]
singleton x = [x]
stringPattern :: Int -> String -> Int
stringPattern field0 "a" = field0
stringPattern field0 _ = field0
