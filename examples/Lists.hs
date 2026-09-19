module Lists where

bump x = x + 1

mapped xs = map bump xs

add x y = x + y

folded xs = foldr add 0 xs

summedFromLeft x = foldl add 0 [x, x]

zipped x = head (zipWith add (x : undefined) (x : undefined))

cond n = if (undefined :: Bool) then [n] else []

scanned x = head (scanl add x (cond x))

always x = True

filtered x = head (filter always (x : undefined))

stream n = n : stream (n + 1)

second (_ : x : _) = x

observe n = second (stream n)

rest n = head (tail (stream n))
