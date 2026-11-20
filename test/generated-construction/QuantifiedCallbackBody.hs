{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
    RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module QuantifiedCallbackBody where
import Data.Proxy
import qualified Memorist.Memorist as M
import qualified QuantifiedApplication as Q

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

data Box f a = Box (f a)
instance forall f a. (Q.ReprK f, Q.ReprK a) => M.Repr (Box f a) where
    type Rep (Box f a) = M.Rep (f a)
    reprM (Box x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a))
        (let deadOperation = M.tval (undefined :: M.Th Int) in M.reprM x)
    abstract x = Box (M.abstract x)

make :: Box Maybe Int -> M.MemoristM (M.MaybeT Int)
make = M.reprM
