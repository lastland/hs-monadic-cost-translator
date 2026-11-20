{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, QuantifiedConstraints,
    RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module RenamedProof where
import Data.Proxy
import qualified Memorist.Memorist as M

class (forall a. M.Repr a => M.Repr (f a)) => Every f
instance (forall a. M.Repr a => M.Repr (f a)) => Every f
deliver :: c => Proxy c -> (c => r) -> r
deliver _ k = k

data Box f a = Box (f a)
instance forall f a. (Every f, M.Repr a) => M.Repr (Box f a) where
    type Rep (Box f a) = M.Rep (f a)
{-# LINE 50 "same-physical-roles.hs" #-}
    reprM (Box x) = deliver (Proxy :: Proxy (M.Repr (f a))) (M.reprM x)
    abstract x = Box (M.abstract x)

make :: Box Maybe Int -> M.MemoristM (M.MaybeT Int)
make = M.reprM
