{-# LANGUAGE TypeFamilies #-}
module ParamRepr where

import qualified Memorist.Memorist as M

data Box a = Box a
instance M.Repr (Box a) where
    type Rep (Box a) = Box a
    reprM = pure
    abstract = id

data Outer a b = Outer (Box b) a
instance M.Repr (Outer a b) where
    type Rep (Outer a b) = Outer a b
    reprM (Outer box tag) = (\result -> Outer result tag) <$> M.reprM box
    abstract = id
