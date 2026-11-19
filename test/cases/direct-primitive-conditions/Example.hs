module Example where

guarded, prefixIf :: Int -> Int -> Int
guarded x y
    | x <= y = x - y
    | Prelude.otherwise = 1
prefixIf x y = if (<=) x y then (-) x y else 1

threeWay :: Int -> Int -> Int
threeWay x y
    | x < y = 0
    | x == y = 1
    | otherwise = 2

shadowed :: Bool -> Bool -> Int
shadowed useGuard otherwise
    | useGuard, otherwise = 1
    | True = if otherwise then 2 else 0

known :: Int -> Int
known x
    | False = error "unreachable guard body"
    | Prelude.True =
        if False then error "unreachable then branch"
        else if otherwise then x else error "unreachable else branch"

mapped :: Int -> [Int] -> [Bool]
mapped lower xs = map ((<=) lower) xs

andIf, orGuard :: Bool -> Bool -> Int
andIf x y = if x && y then 1 else 0
orGuard x y
    | x || y = 1
    | otherwise = 0
