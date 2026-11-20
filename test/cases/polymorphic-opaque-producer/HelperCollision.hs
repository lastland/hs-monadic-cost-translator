{-# LANGUAGE ScopedTypeVariables, KindSignatures #-}
module Example where

data Higher (f :: * -> *) a = Higher Int (Maybe (f a))

-- The result's quantified variable is first in this source signature.
make :: forall a f. Int -> Higher f a
make payload = Higher payload Nothing

-- Both the argument and the shared local compete with generated helper names.
views :: forall a. Int -> (Higher a Int, Higher a Bool)
views makebody =
    let makebody' :: forall f. Higher a f
        makebody' = make makebody
    in (makebody', makebody')
