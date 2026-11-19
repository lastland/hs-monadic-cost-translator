{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}
module Example where

data Stamp (a :: k) = Stamp Int

work :: Int -> Int
work n = n + n

views :: forall t. t -> Int -> (Int, Int)
views _ n =
    let shared :: forall a. Int
        shared = case (Stamp (work n) :: Stamp a) of Stamp result -> result
    in (shared, shared)
