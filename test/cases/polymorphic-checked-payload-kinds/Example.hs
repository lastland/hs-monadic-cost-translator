{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Box f = Box Int (Maybe (f Int Bool))

work :: Int -> Int
work n = n + n

views :: forall t. t -> Int -> (Box (,), Box (,))
views _ n =
    let shared :: forall f. Box f
        shared = Box (work n) (Nothing :: Maybe (f Int Bool))
    in (shared, shared)
