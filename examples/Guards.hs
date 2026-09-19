module Guards where

choose [] = undefined
choose (y : _) | y < 0 = undefined
               | otherwise = y
ordered x = choose (x : undefined)

guarded x = case x : undefined of
    ys | h : _ <- ys
       , let z = h + h
       , z > 0 -> z
    _ -> undefined

fallback 0 = undefined
fallback n = n
numeric x = fallback x

tag 'a' = 1
tag _ = 2

named "ab" = 3
named _ = 4

pick c = if c == 'z' then 10 else 20

observed x = tag 'a' + named ['a', 'b'] + pick 'z'

keep :: Int -> Int
keep x = let unused = error "unused" in if x < x + 1 || undefined then x else unused
