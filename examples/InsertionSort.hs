module InsertionSort where

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x ys@(y : rest)
    | x <= y = x : ys
    | otherwise = y : insert x rest

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert x (insertionSort xs)
