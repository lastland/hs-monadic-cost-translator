{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Packet a = Packet Int (Maybe a)

keep :: a -> a
keep x = x

work :: Int -> Int
work n = n + n

views :: forall t. t -> Int -> (Packet Int, Packet Bool)
views _ n =
    let shared :: forall a. Packet a
        shared = keep (Packet (work n) Nothing)
    in (shared, shared)
