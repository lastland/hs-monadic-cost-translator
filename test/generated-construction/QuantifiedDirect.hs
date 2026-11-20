{-# LANGUAGE FlexibleContexts, FlexibleInstances, QuantifiedConstraints,
    ScopedTypeVariables, TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedDirect where
import qualified Memorist.Memorist as M

class (forall a. M.Repr a => M.Repr (f a)) => ReprF f
instance (forall a. M.Repr a => M.Repr (f a)) => ReprF f

data Higher f a = Higher (f a)
instance (ReprF f, M.Repr a) => M.Repr (Higher f a) where
    type Rep (Higher f a) = M.Rep (f a)
    reprM (Higher x) = M.reprM x
    abstract x = Higher (M.abstract x)
