{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedFunction where
import Data.Proxy
import qualified Memorist.Memorist as M
import qualified QuantifiedApplication as Q

-- Right name and type, but an unsupported ordinary-function wrapper.
throughFunction :: (Bool -> r) -> r
throughFunction callback = callback True
withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ callback = throughFunction (\_ -> callback)

data Box f a = Box (f a)
instance forall f a. (Q.ReprK f, Q.ReprK a) => M.Repr (Box f a) where
    type Rep (Box f a) = M.Rep (f a)
    reprM (Box x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a)) (M.reprM x)
    abstract x = Box (M.abstract x)
make :: Box Maybe Int -> M.MemoristM (M.MaybeT Int)
make = M.reprM
