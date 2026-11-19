{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Packet a = Packet Int (Maybe a)

work :: Int -> Int
work n = n + n

make :: Bool -> Int -> Packet a
make flag n
    | flag = Packet (work n) Nothing
    | otherwise = make True (n + 1)

views :: forall t. t -> Bool -> Int -> (Packet Int, Packet Bool)
views _ flag n =
    let shared :: forall a. Packet a
        shared = choose flag
          where
            choose b = make b n
    in (shared, shared)
