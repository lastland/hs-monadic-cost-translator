{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, TypeFamilies,
    UndecidableInstances #-}
module FamilyEquality where

import Data.Kind (Constraint)
import qualified Memorist.Memorist as M

type family ReprReq a :: Constraint

data Box a = Box (Maybe a)
data BoxT a = BoxT (M.Th (M.MaybeT a))

instance (ReprReq a, ReprReq a ~ M.Repr a) => M.Repr (Box a) where
    type Rep (Box a) = BoxT (M.Rep a)
    reprM (Box x) = BoxT <$> M.lazy (M.reprM x)
    abstract (BoxT x) = Box (M.abstract (M.tval x))
