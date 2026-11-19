{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Packet f = Packet Int (Maybe (f Int))

work :: Int -> Int
work n = n + n

views :: forall t. t -> Int -> (Packet [], Packet Maybe)
views _ n =
    let shared :: forall f. Packet f
        shared = Packet (work n) Nothing
    in (shared, shared)
