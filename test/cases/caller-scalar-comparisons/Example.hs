module Example where

equal, different :: Eq a => a -> a -> Bool
equal x y = x == y
different x y = x /= y

less, lessEqual, greater, greaterEqual :: Ord a => a -> a -> Bool
less x y = x < y
lessEqual x y = x <= y
greater x y = x > y
greaterEqual x y = x >= y

equalViaOrd :: Ord a => a -> a -> Bool
equalViaOrd x y = x == y

skipped :: Eq a => Bool -> a -> a -> Bool
skipped enabled x y = enabled && x == y

suspended :: Eq a => a -> a -> Maybe Bool
suspended x y = Just (x == y)

work :: Int -> Int
work n = if n == 0 then 1 else work 0 + 1
