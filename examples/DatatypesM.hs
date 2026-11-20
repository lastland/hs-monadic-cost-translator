{-# LANGUAGE
    RankNTypes,
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    UndecidableSuperClasses,
    PolyKinds,
    ConstraintKinds,
    QuantifiedConstraints,
    StandaloneKindSignatures,
    StandaloneDeriving,
    ScopedTypeVariables,
    DataKinds,
    TypeOperators,
    NoMonoLocalBinds #-}
-- Runtime-owned types require local instances; generic dictionaries retain intentional constraints.
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module DatatypesM where

import Data.Kind
import Data.Proxy
import Prelude

import qualified Datatypes as Source
import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- ==== Common: sharing and scalar support ====

class Scalar a

instance Scalar Int

instance Scalar Integer

instance Scalar Bool

instance Scalar Float

instance Scalar Double

instance Scalar Char

instance Scalar ()

-- ==== Common: foldable support ====

class FoldableT t where
  foldrT ::
    M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (t a) -> M.MemoristM b
  foldlT ::
    M.Th (M.Th b -> M.Th a -> M.MemoristM b)
    -> M.Th b -> M.Th (t a) -> M.MemoristM b
  concatMapT ::
    M.Th (M.Th a -> M.MemoristM (L.ListT b))
    -> M.Th (t a) -> M.MemoristM (L.ListT b)
  concatMapT tf txs
    = do M.tick
         callback <- M.lazy
                       (pure
                          (\ x rest
                             -> do f <- M.force tf
                                   ys <- M.lazy (f x)
                                   ML.appendM ys rest))
         nil <- M.lazy (pure L.NilT)
         foldrT callback nil txs

instance FoldableT L.ListT where
  foldrT = ML.foldrM
  foldlT = ML.foldlM
  concatMapT = ML.concatMapM

instance FoldableT M.MaybeT where
  foldrT tf z txs
    = do M.tick
         xs <- M.force txs
         case xs of
           M.NothingT -> M.force z
           M.JustT x
             -> do f <- M.force tf
                   f x z
  foldlT tf z txs
    = do M.tick
         xs <- M.force txs
         case xs of
           M.NothingT -> M.force z
           M.JustT x
             -> do f <- M.force tf
                   f z x

instance FoldableT (M.PairT c) where
  foldrT tf z txs
    = do M.tick
         xs <- M.force txs
         case xs of
           M.PairT _ x
             -> do f <- M.force tf
                   f x z
  foldlT tf z txs
    = do M.tick
         xs <- M.force txs
         case xs of
           M.PairT _ x
             -> do f <- M.force tf
                   f z x

-- ==== Common: datatype conversion and demand support ====

instance M.Repr Char where
  type Rep Char = Char
  reprM = pure
  abstract c = c

instance M.Demand Char where
  type Dem Char = Char
  fullDemand c = c

instance M.LBoundDemand Char where
  minDemand c = c

instance M.DemandArb Char where
  arbDemandFrom = pure

class M.Demand a => GeneratedDemand a where
  genDemandBudget :: Int -> a -> QC.Gen (M.Dem a)
  shrinkDemandCompatible :: a -> M.Dem a -> [M.Dem a]
  shrinkDemandCompatible _ _ = []

genField ::
  GeneratedDemand a => Int -> M.Th a -> QC.Gen (M.ThD (M.Dem a))
genField n t
  = if n <= 1 then
        pure M.Skip
    else
        QC.liftArbitrary (genDemandBudget (n - 2) (M.tval t))

shrinkField ::
  GeneratedDemand a => M.Th a -> M.ThD (M.Dem a) -> [M.ThD (M.Dem a)]
shrinkField t = QC.liftShrink (shrinkDemandCompatible (M.tval t))

arbitraryField :: QC.Arbitrary a => Int -> QC.Gen (M.ThD a)
arbitraryField n
  = if n <= 1 then
        pure M.Skip
    else
        QC.liftArbitrary (QC.resize (n - 2) QC.arbitrary)

instance GeneratedDemand Int where
  genDemandBudget _ = pure

instance GeneratedDemand Integer where
  genDemandBudget _ = pure

instance GeneratedDemand Bool where
  genDemandBudget _ = pure

instance GeneratedDemand Float where
  genDemandBudget _ = pure

instance GeneratedDemand Double where
  genDemandBudget _ = pure

instance GeneratedDemand Char where
  genDemandBudget _ = pure

instance GeneratedDemand () where
  genDemandBudget _ = pure

instance GeneratedDemand a => GeneratedDemand (L.ListT a) where
  genDemandBudget _ L.NilT = pure L.NilD
  genDemandBudget n (L.ConsT h t)
    = (L.ConsD <$> genField (n `div` 2) h) <*> genField (n `div` 2) t
  shrinkDemandCompatible (L.ConsT h t) (L.ConsD dh dt)
    = [L.ConsD d dt | d <- shrinkField h dh]
        ++ [L.ConsD dh d | d <- shrinkField t dt]
  shrinkDemandCompatible _ _ = []

instance GeneratedDemand a => GeneratedDemand (M.MaybeT a) where
  genDemandBudget _ M.NothingT = pure M.NothingD
  genDemandBudget n (M.JustT t) = M.JustD <$> genField n t
  shrinkDemandCompatible (M.JustT t) (M.JustD d)
    = [M.JustD s | s <- shrinkField t d]
  shrinkDemandCompatible _ _ = []

instance (GeneratedDemand a, GeneratedDemand b) =>
         GeneratedDemand (M.PairT a b) where
  genDemandBudget n (M.PairT a b)
    = (M.PairD <$> genField (n `div` 2) a) <*> genField (n `div` 2) b
  shrinkDemandCompatible (M.PairT a b) (M.PairD da db)
    = [M.PairD d db | d <- shrinkField a da]
        ++ [M.PairD da d | d <- shrinkField b db]

-- ==== Module-specific: source datatypes and conversions from Datatypes ====

type RepOf :: forall k. k -> k
type RepOf (a :: k) = RepK k a

type RepK :: forall k -> k -> k
type family RepK k a where
  RepK Type a = M.Rep a
  RepK (Type -> Type) [] = L.ListT
  RepK (Type -> Type) Maybe = M.MaybeT
  RepK (Type -> Type -> Type) (,) = M.PairT
  RepK (Type -> Type) Source.Nested = NestedT
  RepK (Type -> Type) Source.Tree = TreeT
  RepK k (f (a :: j)) = RepK (j -> k) f (RepK j a)
  RepK k a = a

type DemOf :: forall k. k -> k
type DemOf (a :: k) = DemK k a

type DemK :: forall k -> k -> k
type family DemK k a where
  DemK Type a = M.Dem a
  DemK (Type -> Type) L.ListT = L.ListD
  DemK (Type -> Type) M.MaybeT = M.MaybeD
  DemK (Type -> Type -> Type) M.PairT = M.PairD
  DemK (Type -> Type) NestedT = NestedD
  DemK (Type -> Type) TreeT = TreeD
  DemK k (f (a :: j)) = DemK (j -> k) f (DemK j a)
  DemK k a = a

type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a where
  ReprReq Type a = M.Repr a
  ReprReq (k -> l) f = ReprF f
  ReprReq k a = ()

type ReprK :: forall k. k -> Constraint
type ReprK (a :: k) = ReprReq k a

type ReprApp :: forall k l. (k -> l) -> k -> Constraint
class (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) => ReprApp f a

instance (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) =>
         ReprApp f a

type ReprF :: forall k l. (k -> l) -> Constraint
class (forall a. ReprK a => ReprApp f a) => ReprF f

instance (forall a. ReprK a => ReprApp f a) => ReprF f

type DemandReq :: forall k -> k -> Constraint
type family DemandReq k a where
  DemandReq Type a = M.Demand a
  DemandReq (k -> l) f = DemandF f
  DemandReq k a = ()

type DemandK :: forall k. k -> Constraint
type DemandK (a :: k) = DemandReq k a

type DemandApp :: forall k l. (k -> l) -> k -> Constraint
class (DemandK (f a),
       DemOf (f a) ~ DemOf f (DemOf a)) => DemandApp f a

instance (DemandK (f a), DemOf (f a) ~ DemOf f (DemOf a)) =>
         DemandApp f a

type DemandF :: forall k l. (k -> l) -> Constraint
class (forall a. DemandK a => DemandApp f a) => DemandF f

instance (forall a. DemandK a => DemandApp f a) => DemandF f

type GeneratedReq :: forall k -> k -> Constraint
type family GeneratedReq k a where
  GeneratedReq Type a = GeneratedDemand a
  GeneratedReq (k -> l) f = GeneratedF f
  GeneratedReq k a = ()

type GeneratedK :: forall k. k -> Constraint
class (DemandK a, GeneratedReq k a) => GeneratedK (a :: k)

instance (DemandK a, GeneratedReq k a) => GeneratedK (a :: k)

type GeneratedF :: forall k l. (k -> l) -> Constraint
class (forall a. GeneratedK a => GeneratedK (f a)) => GeneratedF f

instance (forall a. GeneratedK a => GeneratedK (f a)) =>
         GeneratedF f

type ShapeReq :: (Type -> Constraint)
                 -> forall k -> k -> Constraint
type family ShapeReq c k a where
  ShapeReq c Type a = c a
  ShapeReq c (k -> l) f = ShapeF c f
  ShapeReq c k a = ()

type ShapeK :: (Type -> Constraint) -> forall k. k -> Constraint
class ShapeReq c k a => ShapeK c (a :: k)

instance ShapeReq c k a => ShapeK c (a :: k)

type ShapeF :: (Type -> Constraint)
               -> forall k l. (k -> l) -> Constraint
class (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f

instance (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

data NestedT (a :: Type)
  = EndT (M.Th a) | MoreT (M.Th (NestedT (L.ListT a)))

instance forall (a :: Type).
         ReprK a => M.Repr (Source.Nested a) where
  type Rep (Source.Nested a) = NestedT (RepOf a)
  reprM (Source.End x1) = EndT <$> M.lazy (M.reprM x1)
  reprM (Source.More x1) = MoreT <$> M.lazy (M.reprM x1)
  abstract (EndT x1) = Source.End (M.abstract (M.tval x1))
  abstract (MoreT x1) = Source.More (M.abstract (M.tval x1))

data NestedD (a :: Type)
  = EndD (M.ThD a) | MoreD (M.ThD (NestedD (L.ListD a)))

deriving instance forall (a :: Type). ShapeK Eq a => Eq (NestedD a)

deriving instance forall (a :: Type).
                  ShapeK Show a => Show (NestedD a)

instance forall (a :: Type).
         DemandK a => M.Demand (NestedT a) where
  type Dem (NestedT a) = NestedD (DemOf a)
  fullDemand (EndT x1) = EndD (M.fullDemand x1)
  fullDemand (MoreT x1) = MoreD (M.fullDemand x1)
  usage (EndT x1) (EndD x1d) = M.usage x1 x1d `M.union` M.empty
  usage (MoreT x1) (MoreD x1d) = M.usage x1 x1d `M.union` M.empty
  usage _ _ = M.empty

instance forall (a :: Type).
         DemandK a => M.LBoundDemand (NestedT a) where
  minDemand EndT {} = EndD M.Skip
  minDemand MoreT {} = MoreD M.Skip

instance forall (a :: Type).
         GeneratedK a => GeneratedDemand (NestedT a) where
  genDemandBudget n (EndT x1) = EndD <$> genField (n `div` 1) x1
  genDemandBudget n (MoreT x1) = MoreD <$> genField (n `div` 1) x1
  shrinkDemandCompatible (EndT x1) (EndD x1d)
    = [EndD smaller | smaller <- shrinkField x1 x1d]
  shrinkDemandCompatible (MoreT x1) (MoreD x1d)
    = [MoreD smaller | smaller <- shrinkField x1 x1d]
  shrinkDemandCompatible _ _ = []

genNestedDemand ::
  forall (a :: Type).
  GeneratedK a => Int -> NestedT a -> QC.Gen (NestedD (DemOf a))
genNestedDemand = genDemandBudget

shrinkNestedDemand ::
  forall (a :: Type).
  GeneratedK a =>
  NestedT a -> NestedD (DemOf a) -> [NestedD (DemOf a)]
shrinkNestedDemand = shrinkDemandCompatible

instance forall (a :: Type).
         ShapeK QC.Arbitrary a => QC.Arbitrary (NestedD a) where
  arbitrary
    = QC.sized
        (\ n
           -> QC.oneof
                [EndD <$> arbitraryField (n `div` 1),
                 MoreD <$> arbitraryField (n `div` 1)])
  shrink _ = []

instance forall (a :: Type).
         (GeneratedK a,
          ShapeK QC.Arbitrary (DemOf a),
          ShapeK Show (DemOf a)) =>
         M.DemandArb (NestedT a) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (a :: Type).
         (ReprK a, DemandK (RepOf a)) => M.ReprDemand (Source.Nested a)

instance forall (a :: Type).
         (ReprK a,
          GeneratedK (RepOf a),
          ShapeK QC.Arbitrary (DemOf (RepOf a)),
          ShapeK Show (DemOf (RepOf a))) =>
         M.ReprDemandArb (Source.Nested a)

data TreeT (a :: Type)
  = LeafT | NodeT (M.Th a) (M.Th (TreeT a)) (M.Th (TreeT a))

instance forall (a :: Type).
         ReprK a => M.Repr (Source.Tree a) where
  type Rep (Source.Tree a) = TreeT (RepOf a)
  reprM Source.Leaf = pure LeafT
  reprM (Source.Node x1 x2 x3)
    = ((NodeT <$> M.lazy (M.reprM x1)) <*> M.lazy (M.reprM x2))
        <*> M.lazy (M.reprM x3)
  abstract LeafT = Source.Leaf
  abstract (NodeT x1 x2 x3)
    = Source.Node
        (M.abstract (M.tval x1)) (M.abstract (M.tval x2))
        (M.abstract (M.tval x3))

data TreeD (a :: Type)
  = LeafD | NodeD (M.ThD a) (M.ThD (TreeD a)) (M.ThD (TreeD a))

deriving instance forall (a :: Type). ShapeK Eq a => Eq (TreeD a)

deriving instance forall (a :: Type).
                  ShapeK Show a => Show (TreeD a)

instance forall (a :: Type). DemandK a => M.Demand (TreeT a) where
  type Dem (TreeT a) = TreeD (DemOf a)
  fullDemand LeafT = LeafD
  fullDemand (NodeT x1 x2 x3)
    = NodeD (M.fullDemand x1) (M.fullDemand x2) (M.fullDemand x3)
  usage LeafT LeafD = M.empty
  usage (NodeT x1 x2 x3) (NodeD x1d x2d x3d)
    = M.usage x1 x1d
        `M.union`
          (M.usage x2 x2d `M.union` (M.usage x3 x3d `M.union` M.empty))
  usage _ _ = M.empty

instance forall (a :: Type).
         DemandK a => M.LBoundDemand (TreeT a) where
  minDemand LeafT = LeafD
  minDemand NodeT {} = NodeD M.Skip M.Skip M.Skip

instance forall (a :: Type).
         GeneratedK a => GeneratedDemand (TreeT a) where
  genDemandBudget n LeafT = pure LeafD
  genDemandBudget n (NodeT x1 x2 x3)
    = ((NodeD <$> genField (n `div` 3) x1) <*> genField (n `div` 3) x2)
        <*> genField (n `div` 3) x3
  shrinkDemandCompatible LeafT LeafD = []
  shrinkDemandCompatible (NodeT x1 x2 x3) (NodeD x1d x2d x3d)
    = [NodeD smaller x2d x3d | smaller <- shrinkField x1 x1d]
        ++
          ([NodeD x1d smaller x3d | smaller <- shrinkField x2 x2d]
             ++ [NodeD x1d x2d smaller | smaller <- shrinkField x3 x3d])
  shrinkDemandCompatible _ _ = []

genTreeDemand ::
  forall (a :: Type).
  GeneratedK a => Int -> TreeT a -> QC.Gen (TreeD (DemOf a))
genTreeDemand = genDemandBudget

shrinkTreeDemand ::
  forall (a :: Type).
  GeneratedK a => TreeT a -> TreeD (DemOf a) -> [TreeD (DemOf a)]
shrinkTreeDemand = shrinkDemandCompatible

instance forall (a :: Type).
         ShapeK QC.Arbitrary a => QC.Arbitrary (TreeD a) where
  arbitrary
    = QC.sized
        (\ n
           -> QC.oneof
                [pure LeafD,
                 ((NodeD <$> arbitraryField (n `div` 3))
                    <*> arbitraryField (n `div` 3))
                   <*> arbitraryField (n `div` 3)])
  shrink _ = []

instance forall (a :: Type).
         (GeneratedK a,
          ShapeK QC.Arbitrary (DemOf a),
          ShapeK Show (DemOf a)) =>
         M.DemandArb (TreeT a) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (a :: Type).
         (ReprK a, DemandK (RepOf a)) => M.ReprDemand (Source.Tree a)

instance forall (a :: Type).
         (ReprK a,
          GeneratedK (RepOf a),
          ShapeK QC.Arbitrary (DemOf (RepOf a)),
          ShapeK Show (DemOf (RepOf a))) =>
         M.ReprDemandArb (Source.Tree a)

-- ==== Module-specific: translated bindings from Datatypes ====

readNestedM ::
  forall (a :: Type). M.Th (NestedT a) -> M.MemoristM a
readNestedM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         MoreT field0
           -> do matched'' <- M.force field0
                 case matched'' of
                   EndT field1
                     -> do matched''' <- M.force field1
                           case matched''' of
                             L.ConsT x _ -> M.force x
                             _ -> error "source pattern match failure"
                   _ -> error "source pattern match failure"
         _ -> error "source pattern match failure"

sampleM :: M.MemoristM Integer
sampleM
  = do arg0 <- M.lazy
                 (do arg1 <- M.lazy
                               (do arg2 <- M.lazy
                                             (do arg3 <- M.lazy (pure (7 :: Integer))
                                                 arg4 <- M.lazy
                                                           (do arg5 <- M.lazy (pure (8 :: Integer))
                                                               arg6 <- M.lazy
                                                                         (do arg7 <- M.lazy
                                                                                       (pure
                                                                                          (9 ::
                                                                                             Integer))
                                                                             arg8 <- M.lazy
                                                                                       (pure L.NilT)
                                                                             pure
                                                                               (L.ConsT arg7 arg8))
                                                               pure (L.ConsT arg5 arg6))
                                                 pure (L.ConsT arg3 arg4))
                                   pure (EndT arg2))
                     pure (MoreT arg1))
       readNestedM arg0

treeSumM ::
  forall (t :: Type). Num t => M.Th (TreeT t) -> M.MemoristM t
treeSumM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         LeafT -> pure 0
         NodeT a' l r
           -> do arg0' <- M.lazy
                            (do arg3 <- M.lazy (treeSumM l)
                                aValue <- M.force a'
                                rhs' <- M.force arg3
                                pure (aValue + rhs'))
                 arg1 <- M.lazy (treeSumM r)
                 lhs <- M.force arg0'
                 rhs <- M.force arg1
                 pure (lhs + rhs)

totalM :: forall (t :: Type). Num t => M.Th t -> M.MemoristM t
totalM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy (pure LeafT)
                     arg3 <- M.lazy (pure LeafT)
                     pure (NodeT x arg2 arg3))
       treeSumM arg0

deepM :: forall (t :: Type). Num t => M.Th t -> M.MemoristM t
deepM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy
                               (do arg4 <- M.lazy
                                             (do arg8 <- M.lazy (pure 1)
                                                 xValue <- M.force x
                                                 rhs <- M.force arg8
                                                 pure (xValue + rhs))
                                   arg5 <- M.lazy (pure LeafT)
                                   arg6 <- M.lazy (pure LeafT)
                                   pure (NodeT arg4 arg5 arg6))
                     arg3 <- M.lazy (pure LeafT)
                     pure (NodeT x arg2 arg3))
       treeSumM arg0

growM :: forall (t :: Type). M.Th t -> M.MemoristM (TreeT t)
growM x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (growM x)
       arg2 <- M.lazy (growM x)
       pure (NodeT x arg1 arg2)
