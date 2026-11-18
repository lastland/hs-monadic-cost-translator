{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num t => t -> t
work n = n + n

views :: forall a b. (Num a, Num b) => a -> b -> Int -> ((a, b), (a, b))
views _ _ count =
    let shared :: forall x y. (Num x, Num y) => (x, y)
        shared = (work (if count < 0 then error "left failure" else 5),
                  work (if count == 0 then error "right failure" else 7))
    in (shared :: (a, b), shared :: (a, b))
