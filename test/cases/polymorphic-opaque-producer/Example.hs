{-# LANGUAGE ScopedTypeVariables, KindSignatures #-}
module Example where

data Higher (f :: * -> *) a = Higher Int (Maybe (f a))

make :: Int -> Higher f a
make n = Higher n Nothing

views :: forall f. Int -> (Higher f Int, Higher f Bool)
views n =
    let shared :: forall a. Higher f a
        shared = make n
    in (shared, shared)
