{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module FieldChain where
import ContextRepr (Tagged)
import qualified Memorist.Memorist as M

data Outer a = Outer (Tagged Int a)
instance Ord a => M.Repr (Outer a) where
    type Rep (Outer a) = Tagged Int Bool
    reprM (Outer t) = M.reprM t
    abstract = undefined

positive :: Outer Bool -> M.MemoristM (Tagged Int Bool)
positive = M.reprM

data ViaIntegral a = ViaIntegral a
instance Integral a => M.Repr (ViaIntegral a) where
    type Rep (ViaIntegral a) = Bool
    reprM (ViaIntegral x) = pure (x == x)
    abstract = undefined

integral :: ViaIntegral Int -> M.MemoristM Bool
integral = M.reprM

class M.Repr a => Represented a where
    tag :: a -> Bool
instance Represented Int where
    tag _ = True

data Child a = Child a
instance Represented a => M.Repr (Child a) where
    type Rep (Child a) = M.Rep a
    reprM (Child x) = M.reprM x
    abstract = undefined

projectRepr :: Child Int -> M.MemoristM Int
projectRepr = M.reprM

-- This home DFun builds its superclass by projecting its own argument.
class Eq a => ViaOrd a where
    via :: a -> Bool
instance Ord a => ViaOrd [a] where
    via _ = True

data Indirect a = Indirect a
instance ViaOrd a => M.Repr (Indirect a) where
    type Rep (Indirect a) = Bool
    reprM (Indirect x) = pure (x == x)
    abstract = undefined
