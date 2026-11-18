{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Packet a = Packet Int (Maybe a)

views :: forall t. t -> Int -> (Packet Int, Packet Bool)
views _ _ =
    let shared :: forall a. Packet a
        shared = Packet
            (let marker :: Maybe a
                 marker = Nothing
             in case marker of Nothing -> 1; Just _ -> 2)
            Nothing
    in (shared, shared)
