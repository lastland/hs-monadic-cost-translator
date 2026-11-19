{-# LANGUAGE DataKinds, PolyKinds, ScopedTypeVariables #-}
module Example where

data Packet (f :: k -> *) (a :: k) = Packet Int (Maybe (f a))
data Index (a :: Bool) = Index Int

work :: Int -> Int
work n = n + n

views :: forall t. t -> Int -> (Packet Maybe Int, Packet Index 'True)
views _ n =
    let shared :: forall f a. Packet f a
        shared = Packet (work n) (Nothing :: Maybe (f a))
    in (shared, shared)
