{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num t => t -> t
work n = n + n

views :: forall a. Integral a => a -> Int -> ((a, Int), (a, Int))
views _ count =
    let shared :: forall key b. Integral key => ((key, Int), [b])
        shared = ((work ((if count < 0 then 9 else 15) `quot` 3),
                   work (if count == 0 then 11 else 7)), [])
    in case (shared :: ((a, Int), [Bool])) of
        (withBools, _) -> case (shared :: ((a, Int), [Char])) of
            (withChars, _) -> (withBools, withChars)
