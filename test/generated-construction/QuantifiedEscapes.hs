{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
    InstanceSigs, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeFamilies,
    UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedEscapes where
import Data.Proxy
import qualified Memorist.Memorist as M
import qualified QuantifiedApplication as Q

-- An available exact helper makes evidence ownership the failing boundary.
withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

openApplication :: forall f a. (Q.ReprK f, Q.ReprK a) =>
    f a -> M.MemoristM (M.Rep (f a))
openApplication x = withConstraint (Proxy :: Proxy (Q.ReprApp f a)) (M.reprM x)
openHome :: forall f a. (Q.ReprK f, Q.ReprK a) =>
    Q.Higher f a -> M.MemoristM (Q.HigherT (Q.RepOf f) (Q.RepOf a))
openHome = M.reprM
openGiven :: M.Repr a => a -> M.MemoristM (M.Rep a)
openGiven = M.reprM
observer :: forall f a. (Q.ReprK f, Q.ReprK a) => M.Rep (f a) -> f a
observer x = withConstraint (Proxy :: Proxy (Q.ReprApp f a)) (M.abstract x)

data Captured f a = Captured (f a)
instance forall f a. (Q.ReprK f, Q.ReprK a) => M.Repr (Captured f a) where
    type Rep (Captured f a) = M.Rep (f a)
    reprM :: forall s m. M.Memorist s m => Captured f a -> m (M.Rep (f a))
    reprM (Captured x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a))
        (let helper :: f a -> m (M.Rep (f a))
             helper y = M.reprM y
         in helper x)
    abstract x = Captured (M.abstract x)

data Dead f a = Dead (f a)
instance forall f a. (Q.ReprK f, Q.ReprK a) => M.Repr (Dead f a) where
    type Rep (Dead f a) = M.Rep (f a)
    reprM (Dead x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a))
        (let unused = M.reprM x :: M.MemoristM (M.Rep (f a)) in M.reprM x)
    abstract x = Dead (M.abstract x)

data Guarded f a = Guarded (M.Th a) (f a)
instance forall f a. (Eq a, Q.ReprK f, Q.ReprK a) => M.Repr (Guarded f a) where
    type Rep (Guarded f a) = M.Rep (f a)
    reprM (Guarded thunk x) = withConstraint (Proxy :: Proxy (Q.ReprApp f a))
        (let deadProtectedComparison = thunk == thunk in M.reprM x)
    abstract _ = undefined
concreteProtected :: Guarded Maybe Int -> M.MemoristM (M.MaybeT Int)
concreteProtected = M.reprM
