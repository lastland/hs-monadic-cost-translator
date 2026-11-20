{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, KindSignatures,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedWrapper where
import Data.Kind
import Data.Proxy
import qualified Memorist.Memorist as M
import qualified QuantifiedApplication as Q

-- A conservative structural boundary, not a claim this wrapper is unsafe.
data Wrapped (c :: Constraint) r = Wrapped (c => r)
consume :: c => Wrapped c r -> r
consume (Wrapped k) = k
withConstraint :: forall c r. c => Proxy c -> (c => r) -> r
withConstraint _ k = consume (Wrapped k :: Wrapped c r)
data Box f a = Box (f a)
instance forall f a. (Q.ReprK f, Q.ReprK a) => M.Repr (Box f a) where
    type Rep (Box f a) = M.Rep (f a)
    reprM (Box x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a)) (M.reprM x)
    abstract x = Box (M.abstract x)
make :: Box Maybe Int -> M.MemoristM (M.MaybeT Int)
make = M.reprM
