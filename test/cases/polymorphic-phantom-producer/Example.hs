{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}
module Example where

data Parcel (f :: * -> *) a = Missing a | Packed a Int Int

work :: Int -> Int
work n = n + n

make :: a -> Bool -> Int -> Int -> Parcel f a
make tag present left right =
    if present then Packed tag (work left) (work right) else Missing tag

views :: forall a. a -> Bool -> Int -> Int -> (Parcel [] a, Parcel Maybe a)
views tag present left right =
    let shared :: forall f. Parcel f a
        shared = make tag present left right
    in (shared, shared)
