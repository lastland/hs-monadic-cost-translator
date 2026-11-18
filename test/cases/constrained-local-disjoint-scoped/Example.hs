{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num t => t -> t
work n = n + n

views :: forall a. Integral a => a -> Int -> (((a, Int), (a, Int)), ((a, Integer), (a, Integer)))
views _ count =
    let shared :: forall x y. (Num x, Num y) => (x, y)
        shared = (work (if count < 0 then 3 else 5),
                  work (if count == 0 then 11 else 7))
    in ((shared :: (a, Int), shared :: (a, Int)),
        (shared :: (a, Integer), shared :: (a, Integer)))
