{-# LANGUAGE TypeFamilies #-}
module ContextRepr where

import qualified Memorist.Memorist as M

data Box a = Box a
instance M.Repr a => M.Repr (Box a) where
    type Rep (Box a) = Box a
    reprM = pure
    abstract = id

data Nested a = Nested a
instance M.Repr a => M.Repr (Nested a) where
    type Rep (Nested a) = Nested (M.Rep a)
    reprM (Nested x) = Nested <$> M.reprM x
    abstract = undefined

data Tagged a b = Tagged a b
instance (Eq b, M.Repr a) => M.Repr (Tagged a b) where
    type Rep (Tagged a b) = Tagged (M.Rep a) Bool
    reprM (Tagged x y) = (\r -> Tagged r (y == y)) <$> M.reprM x
    abstract = undefined
