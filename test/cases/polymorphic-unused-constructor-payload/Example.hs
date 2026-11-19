{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Box a = Box Int (Maybe a)

views :: Int -> Int
views n =
    let shared :: forall a. Box a
        shared = Box (let bad = \x -> x in n) Nothing
    in n
