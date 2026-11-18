{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Packet a = Packet Int (Maybe a)

work :: Int -> Int
work n = n + n

choose :: Int -> Bool
choose n = n < 0

views :: forall t. t -> Int -> (Packet Int, Packet Bool)
views _ n =
    let shared :: forall a. Packet a
        shared = if choose n then Packet (work n) Nothing else Packet (work (n + 1)) Nothing
    in (shared, shared)
