{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, PolyKinds,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
    TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module TreeRepr where

import Data.Kind
import qualified TreeReprSource as Source
import qualified Memorist.Memorist as M

type RepOf :: forall k. k -> k
type RepOf (a :: k) = RepK k a

type RepK :: forall k -> k -> k
type family RepK k a where
    RepK Type a = M.Rep a
    RepK (Type -> Type) Source.Tree = TreeT
    RepK k (f (a :: j)) = RepK (j -> k) f (RepK j a)
    RepK k a = a

type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a where
    ReprReq Type a = M.Repr a
    ReprReq (k -> l) f = ReprF f
    ReprReq k a = ()

type ReprK :: forall k. k -> Constraint
type ReprK (a :: k) = ReprReq k a

type ReprApp :: forall k l. (k -> l) -> k -> Constraint
class (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) => ReprApp f a
instance (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) => ReprApp f a

type ReprF :: forall k l. (k -> l) -> Constraint
class (forall a. ReprK a => ReprApp f a) => ReprF f
instance (forall a. ReprK a => ReprApp f a) => ReprF f

data TreeT a = LeafT | NodeT (M.Th a) (M.Th (TreeT a)) (M.Th (TreeT a))

instance forall a. ReprK a => M.Repr (Source.Tree a) where
    type Rep (Source.Tree a) = TreeT (RepOf a)
    reprM Source.Leaf = pure LeafT
    reprM (Source.Node value left right) =
        NodeT <$> M.lazy (M.reprM value) <*> M.lazy (M.reprM left) <*> M.lazy (M.reprM right)
    abstract LeafT = Source.Leaf
    abstract (NodeT value left right) =
        Source.Node (M.abstract (M.tval value)) (M.abstract (M.tval left)) (M.abstract (M.tval right))

intCaller :: Source.Tree Int -> M.MemoristM (TreeT Int)
intCaller = M.reprM

boolCaller :: Source.Tree Bool -> M.MemoristM (TreeT Bool)
boolCaller = M.reprM
