{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num t => t -> t
work n = n + n

views :: forall a. Integral a => a -> Int -> ((a, Int), (a, Int))
views _ count =
    let outer :: forall b. Num b => (b, Int)
        outer =
            let inner :: forall x y. (Num x, Num y) => (x, y)
                inner = (work (if count < 0 then 3 else 5),
                         work (if count == 0 then 11 else 7))
            in inner :: (b, Int)
    in (outer :: (a, Int), outer :: (a, Int))
