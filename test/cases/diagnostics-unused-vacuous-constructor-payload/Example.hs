{-# LANGUAGE ScopedTypeVariables, NoPolyKinds #-}
module Example where

views :: Int -> Int
views n =
    let shared :: forall a. Maybe Int
        shared = Just (let bad = \x -> x in n)
    in n
