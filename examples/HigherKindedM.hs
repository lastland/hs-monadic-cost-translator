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

module HigherKindedM where

import Data.Kind
import Data.Proxy
import Prelude

import qualified HigherKinded as Source
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

-- ==== Module-specific: source datatypes and conversions from HigherKinded ====

type RepOf :: forall k. k -> k
type RepOf (a :: k) = RepK k a

type RepK :: forall k -> k -> k
type family RepK k a where
  RepK Type a = M.Rep a
  RepK (Type -> Type) [] = L.ListT
  RepK (Type -> Type) Maybe = M.MaybeT
  RepK (Type -> Type -> Type) (,) = M.PairT
  RepK ((Type -> Type) -> Type -> Type) Source.Mixed = MixedT
  RepK (Type -> Type) Source.Layer = LayerT
  RepK ((Type -> Type) -> Type) Source.Nest = NestT
  RepK ((Type -> Type) -> Type) Source.Fix = FixT
  RepK ((k -> Type) -> k -> Type) Source.Higher = HigherT
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
  DemK ((Type -> Type) -> Type -> Type) MixedT = MixedD
  DemK (Type -> Type) LayerT = LayerD
  DemK ((Type -> Type) -> Type) NestT = NestD
  DemK ((Type -> Type) -> Type) FixT = FixD
  DemK ((k -> Type) -> k -> Type) HigherT = HigherD
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

data MixedT (f :: Type -> Type) (a :: Type)
  = LeafT (M.Th a) | BranchT (M.Th (f (MixedT f (L.ListT a))))

instance forall (f :: Type -> Type) (a :: Type).
         (ReprK f, ReprK a) => M.Repr (Source.Mixed f a) where
  type Rep (Source.Mixed f a) = MixedT (RepOf f) (RepOf a)
  reprM (Source.Leaf x1) = LeafT <$> M.lazy (M.reprM x1)
  reprM (Source.Branch x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Mixed f ([] a))))
        (BranchT <$> M.lazy (M.reprM x1))
  abstract (LeafT x1) = Source.Leaf (M.abstract (M.tval x1))
  abstract (BranchT x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Mixed f ([] a))))
        (Source.Branch (M.abstract (M.tval x1)))

data MixedD (f :: Type -> Type) (a :: Type)
  = LeafD (M.ThD a) | BranchD (M.ThD (f (MixedD f (L.ListD a))))

deriving instance forall (f :: Type -> Type) (a :: Type).
                  (ShapeK Eq f, ShapeK Eq a) => Eq (MixedD f a)

deriving instance forall (f :: Type -> Type) (a :: Type).
                  (ShapeK Show f, ShapeK Show a) => Show (MixedD f a)

instance forall (f :: Type -> Type) (a :: Type).
         (DemandK f, DemandK a) => M.Demand (MixedT f a) where
  type Dem (MixedT f a) = MixedD (DemOf f) (DemOf a)
  fullDemand (LeafT x1) = LeafD (M.fullDemand x1)
  fullDemand (BranchT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (MixedT f (L.ListT a))))
        (BranchD (M.fullDemand x1))
  usage (LeafT x1) (LeafD x1d) = M.usage x1 x1d `M.union` M.empty
  usage (BranchT x1) (BranchD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (MixedT f (L.ListT a))))
        (M.usage x1 x1d `M.union` M.empty)
  usage _ _ = M.empty

instance forall (f :: Type -> Type) (a :: Type).
         (DemandK f, DemandK a) => M.LBoundDemand (MixedT f a) where
  minDemand LeafT {} = LeafD M.Skip
  minDemand BranchT {} = BranchD M.Skip

instance forall (f :: Type -> Type) (a :: Type).
         (GeneratedK f, GeneratedK a) => GeneratedDemand (MixedT f a) where
  genDemandBudget n (LeafT x1) = LeafD <$> genField (n `div` 1) x1
  genDemandBudget n (BranchT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (MixedT f (L.ListT a))))
        (BranchD <$> genField (n `div` 1) x1)
  shrinkDemandCompatible (LeafT x1) (LeafD x1d)
    = [LeafD smaller | smaller <- shrinkField x1 x1d]
  shrinkDemandCompatible (BranchT x1) (BranchD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (MixedT f (L.ListT a))))
        [BranchD smaller | smaller <- shrinkField x1 x1d]
  shrinkDemandCompatible _ _ = []

genMixedDemand ::
  forall (f :: Type -> Type) (a :: Type).
  (GeneratedK f, GeneratedK a) =>
  Int -> MixedT f a -> QC.Gen (MixedD (DemOf f) (DemOf a))
genMixedDemand = genDemandBudget

shrinkMixedDemand ::
  forall (f :: Type -> Type) (a :: Type).
  (GeneratedK f, GeneratedK a) =>
  MixedT f a
  -> MixedD (DemOf f) (DemOf a) -> [MixedD (DemOf f) (DemOf a)]
shrinkMixedDemand = shrinkDemandCompatible

instance forall (f :: Type -> Type) (a :: Type).
         (ShapeK QC.Arbitrary f, ShapeK QC.Arbitrary a) =>
         QC.Arbitrary (MixedD f a) where
  arbitrary
    = QC.sized
        (\ n
           -> QC.oneof
                [LeafD <$> arbitraryField (n `div` 1),
                 BranchD <$> arbitraryField (n `div` 1)])
  shrink _ = []

instance forall (f :: Type -> Type) (a :: Type).
         (GeneratedK f,
          GeneratedK a,
          ShapeK QC.Arbitrary (DemOf f),
          ShapeK Show (DemOf f),
          ShapeK QC.Arbitrary (DemOf a),
          ShapeK Show (DemOf a)) =>
         M.DemandArb (MixedT f a) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (f :: Type -> Type) (a :: Type).
         (ReprK f, DemandK (RepOf f), ReprK a, DemandK (RepOf a)) =>
         M.ReprDemand (Source.Mixed f a)

instance forall (f :: Type -> Type) (a :: Type).
         (ReprK f,
          GeneratedK (RepOf f),
          ShapeK QC.Arbitrary (DemOf (RepOf f)),
          ShapeK Show (DemOf (RepOf f)),
          ReprK a,
          GeneratedK (RepOf a),
          ShapeK QC.Arbitrary (DemOf (RepOf a)),
          ShapeK Show (DemOf (RepOf a))) =>
         M.ReprDemandArb (Source.Mixed f a)

data LayerT (a :: Type) = LayerT (M.Th a)

instance forall (a :: Type).
         ReprK a => M.Repr (Source.Layer a) where
  type Rep (Source.Layer a) = LayerT (RepOf a)
  reprM (Source.Layer x1) = LayerT <$> M.lazy (M.reprM x1)
  abstract (LayerT x1) = Source.Layer (M.abstract (M.tval x1))

data LayerD (a :: Type) = LayerD (M.ThD a)

deriving instance forall (a :: Type). ShapeK Eq a => Eq (LayerD a)

deriving instance forall (a :: Type).
                  ShapeK Show a => Show (LayerD a)

instance forall (a :: Type). DemandK a => M.Demand (LayerT a) where
  type Dem (LayerT a) = LayerD (DemOf a)
  fullDemand (LayerT x1) = LayerD (M.fullDemand x1)
  usage (LayerT x1) (LayerD x1d) = M.usage x1 x1d `M.union` M.empty

instance forall (a :: Type).
         DemandK a => M.LBoundDemand (LayerT a) where
  minDemand LayerT {} = LayerD M.Skip

instance forall (a :: Type).
         GeneratedK a => GeneratedDemand (LayerT a) where
  genDemandBudget n (LayerT x1) = LayerD <$> genField (n `div` 1) x1
  shrinkDemandCompatible (LayerT x1) (LayerD x1d)
    = [LayerD smaller | smaller <- shrinkField x1 x1d]

genLayerDemand ::
  forall (a :: Type).
  GeneratedK a => Int -> LayerT a -> QC.Gen (LayerD (DemOf a))
genLayerDemand = genDemandBudget

shrinkLayerDemand ::
  forall (a :: Type).
  GeneratedK a => LayerT a -> LayerD (DemOf a) -> [LayerD (DemOf a)]
shrinkLayerDemand = shrinkDemandCompatible

instance forall (a :: Type).
         ShapeK QC.Arbitrary a => QC.Arbitrary (LayerD a) where
  arbitrary = QC.sized (\ n -> LayerD <$> arbitraryField (n `div` 1))
  shrink _ = []

instance forall (a :: Type).
         (GeneratedK a,
          ShapeK QC.Arbitrary (DemOf a),
          ShapeK Show (DemOf a)) =>
         M.DemandArb (LayerT a) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (a :: Type).
         (ReprK a, DemandK (RepOf a)) => M.ReprDemand (Source.Layer a)

instance forall (a :: Type).
         (ReprK a,
          GeneratedK (RepOf a),
          ShapeK QC.Arbitrary (DemOf (RepOf a)),
          ShapeK Show (DemOf (RepOf a))) =>
         M.ReprDemandArb (Source.Layer a)

data NestT (f :: Type -> Type)
  = NestT (M.Th (M.MaybeT (f (NestT f))))

instance forall (f :: Type -> Type).
         ReprK f => M.Repr (Source.Nest f) where
  type Rep (Source.Nest f) = NestT (RepOf f)
  reprM (Source.Nest x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Nest f)))
        (NestT <$> M.lazy (M.reprM x1))
  abstract (NestT x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Nest f)))
        (Source.Nest (M.abstract (M.tval x1)))

data NestD (f :: Type -> Type)
  = NestD (M.ThD (M.MaybeD (f (NestD f))))

deriving instance forall (f :: Type -> Type).
                  ShapeK Eq f => Eq (NestD f)

deriving instance forall (f :: Type -> Type).
                  ShapeK Show f => Show (NestD f)

instance forall (f :: Type -> Type).
         DemandK f => M.Demand (NestT f) where
  type Dem (NestT f) = NestD (DemOf f)
  fullDemand (NestT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (NestT f))) (NestD (M.fullDemand x1))
  usage (NestT x1) (NestD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (NestT f)))
        (M.usage x1 x1d `M.union` M.empty)

instance forall (f :: Type -> Type).
         DemandK f => M.LBoundDemand (NestT f) where
  minDemand NestT {} = NestD M.Skip

instance forall (f :: Type -> Type).
         GeneratedK f => GeneratedDemand (NestT f) where
  genDemandBudget n (NestT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (NestT f)))
        (NestD <$> genField (n `div` 1) x1)
  shrinkDemandCompatible (NestT x1) (NestD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (NestT f)))
        [NestD smaller | smaller <- shrinkField x1 x1d]

genNestDemand ::
  forall (f :: Type -> Type).
  GeneratedK f => Int -> NestT f -> QC.Gen (NestD (DemOf f))
genNestDemand = genDemandBudget

shrinkNestDemand ::
  forall (f :: Type -> Type).
  GeneratedK f => NestT f -> NestD (DemOf f) -> [NestD (DemOf f)]
shrinkNestDemand = shrinkDemandCompatible

instance forall (f :: Type -> Type).
         ShapeK QC.Arbitrary f => QC.Arbitrary (NestD f) where
  arbitrary = QC.sized (\ n -> NestD <$> arbitraryField (n `div` 1))
  shrink _ = []

instance forall (f :: Type -> Type).
         (GeneratedK f,
          ShapeK QC.Arbitrary (DemOf f),
          ShapeK Show (DemOf f)) =>
         M.DemandArb (NestT f) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (f :: Type -> Type).
         (ReprK f, DemandK (RepOf f)) => M.ReprDemand (Source.Nest f)

instance forall (f :: Type -> Type).
         (ReprK f,
          GeneratedK (RepOf f),
          ShapeK QC.Arbitrary (DemOf (RepOf f)),
          ShapeK Show (DemOf (RepOf f))) =>
         M.ReprDemandArb (Source.Nest f)

data FixT (f :: Type -> Type) = InT (M.Th (f (FixT f)))

instance forall (f :: Type -> Type).
         ReprK f => M.Repr (Source.Fix f) where
  type Rep (Source.Fix f) = FixT (RepOf f)
  reprM (Source.In x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Fix f)))
        (InT <$> M.lazy (M.reprM x1))
  abstract (InT x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f (Source.Fix f)))
        (Source.In (M.abstract (M.tval x1)))

data FixD (f :: Type -> Type) = InD (M.ThD (f (FixD f)))

deriving instance forall (f :: Type -> Type).
                  ShapeK Eq f => Eq (FixD f)

deriving instance forall (f :: Type -> Type).
                  ShapeK Show f => Show (FixD f)

instance forall (f :: Type -> Type).
         DemandK f => M.Demand (FixT f) where
  type Dem (FixT f) = FixD (DemOf f)
  fullDemand (InT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (FixT f))) (InD (M.fullDemand x1))
  usage (InT x1) (InD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (FixT f)))
        (M.usage x1 x1d `M.union` M.empty)

instance forall (f :: Type -> Type).
         DemandK f => M.LBoundDemand (FixT f) where
  minDemand InT {} = InD M.Skip

instance forall (f :: Type -> Type).
         GeneratedK f => GeneratedDemand (FixT f) where
  genDemandBudget n (InT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (FixT f)))
        (InD <$> genField (n `div` 1) x1)
  shrinkDemandCompatible (InT x1) (InD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f (FixT f)))
        [InD smaller | smaller <- shrinkField x1 x1d]

genFixDemand ::
  forall (f :: Type -> Type).
  GeneratedK f => Int -> FixT f -> QC.Gen (FixD (DemOf f))
genFixDemand = genDemandBudget

shrinkFixDemand ::
  forall (f :: Type -> Type).
  GeneratedK f => FixT f -> FixD (DemOf f) -> [FixD (DemOf f)]
shrinkFixDemand = shrinkDemandCompatible

instance forall (f :: Type -> Type).
         ShapeK QC.Arbitrary f => QC.Arbitrary (FixD f) where
  arbitrary = QC.sized (\ n -> InD <$> arbitraryField (n `div` 1))
  shrink _ = []

instance forall (f :: Type -> Type).
         (GeneratedK f,
          ShapeK QC.Arbitrary (DemOf f),
          ShapeK Show (DemOf f)) =>
         M.DemandArb (FixT f) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (f :: Type -> Type).
         (ReprK f, DemandK (RepOf f)) => M.ReprDemand (Source.Fix f)

instance forall (f :: Type -> Type).
         (ReprK f,
          GeneratedK (RepOf f),
          ShapeK QC.Arbitrary (DemOf (RepOf f)),
          ShapeK Show (DemOf (RepOf f))) =>
         M.ReprDemandArb (Source.Fix f)

data HigherT (f :: k -> Type) (a :: k) = HigherT (M.Th (f a))

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (ReprK f, ReprK a) => M.Repr (Source.Higher f a) where
  type Rep (Source.Higher f a) = HigherT (RepOf f) (RepOf a)
  reprM (Source.Higher x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f a)) (HigherT <$> M.lazy (M.reprM x1))
  abstract (HigherT x1)
    = withConstraint
        (Proxy :: Proxy (ReprApp f a))
        (Source.Higher (M.abstract (M.tval x1)))

data HigherD (f :: k -> Type) (a :: k) = HigherD (M.ThD (f a))

deriving instance forall (k :: Type) (f :: k -> Type) (a :: k).
                  (ShapeK Eq f, ShapeK Eq a) => Eq (HigherD f a)

deriving instance forall (k :: Type) (f :: k -> Type) (a :: k).
                  (ShapeK Show f, ShapeK Show a) => Show (HigherD f a)

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (DemandK f, DemandK a) => M.Demand (HigherT f a) where
  type Dem (HigherT f a) = HigherD (DemOf f) (DemOf a)
  fullDemand (HigherT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f a)) (HigherD (M.fullDemand x1))
  usage (HigherT x1) (HigherD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f a)) (M.usage x1 x1d `M.union` M.empty)

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (DemandK f, DemandK a) => M.LBoundDemand (HigherT f a) where
  minDemand HigherT {} = HigherD M.Skip

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (GeneratedK f, GeneratedK a) => GeneratedDemand (HigherT f a) where
  genDemandBudget n (HigherT x1)
    = withConstraint
        (Proxy :: Proxy (DemandApp f a))
        (HigherD <$> genField (n `div` 1) x1)
  shrinkDemandCompatible (HigherT x1) (HigherD x1d)
    = withConstraint
        (Proxy :: Proxy (DemandApp f a))
        [HigherD smaller | smaller <- shrinkField x1 x1d]

genHigherDemand ::
  forall (k :: Type) (f :: k -> Type) (a :: k).
  (GeneratedK f, GeneratedK a) =>
  Int -> HigherT f a -> QC.Gen (HigherD (DemOf f) (DemOf a))
genHigherDemand = genDemandBudget

shrinkHigherDemand ::
  forall (k :: Type) (f :: k -> Type) (a :: k).
  (GeneratedK f, GeneratedK a) =>
  HigherT f a
  -> HigherD (DemOf f) (DemOf a) -> [HigherD (DemOf f) (DemOf a)]
shrinkHigherDemand = shrinkDemandCompatible

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (ShapeK QC.Arbitrary f, ShapeK QC.Arbitrary a) =>
         QC.Arbitrary (HigherD f a) where
  arbitrary
    = QC.sized (\ n -> HigherD <$> arbitraryField (n `div` 1))
  shrink _ = []

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (GeneratedK f,
          GeneratedK a,
          ShapeK QC.Arbitrary (DemOf f),
          ShapeK Show (DemOf f),
          ShapeK QC.Arbitrary (DemOf a),
          ShapeK Show (DemOf a)) =>
         M.DemandArb (HigherT f a) where
  arbDemandFrom x = QC.sized (`genDemandBudget` x)

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (ReprK f, DemandK (RepOf f), ReprK a, DemandK (RepOf a)) =>
         M.ReprDemand (Source.Higher f a)

instance forall (k :: Type) (f :: k -> Type) (a :: k).
         (ReprK f,
          GeneratedK (RepOf f),
          ShapeK QC.Arbitrary (DemOf (RepOf f)),
          ShapeK Show (DemOf (RepOf f)),
          ReprK a,
          GeneratedK (RepOf a),
          ShapeK QC.Arbitrary (DemOf (RepOf a)),
          ShapeK Show (DemOf (RepOf a))) =>
         M.ReprDemandArb (Source.Higher f a)

-- ==== Module-specific: translated bindings from HigherKinded ====

taggedM ::
  forall (a :: Type). M.Th a -> M.MemoristM (HigherT LayerT a)
taggedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (pure (LayerT x))
       pure (HigherT arg0)

nestedM ::
  forall (t :: Type). Num t => M.Th t -> M.MemoristM (NestT L.ListT)
nestedM n
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy
                               (do arg2 <- M.lazy
                                             (do arg4 <- M.lazy
                                                           (do arg6 <- M.lazy (pure 1)
                                                               nValue <- M.force n
                                                               rhs <- M.force arg6
                                                               pure (nValue + rhs))
                                                 nestedM arg4)
                                   arg3 <- M.lazy (pure L.NilT)
                                   pure (L.ConsT arg2 arg3))
                     pure (M.JustT arg1))
       pure (NestT arg0)

forestM ::
  forall (t :: Type). Num t => M.Th t -> M.MemoristM (FixT L.ListT)
forestM n
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy
                               (do arg5 <- M.lazy
                                             (do arg7 <- M.lazy (pure 1)
                                                 nValue <- M.force n
                                                 rhs <- M.force arg7
                                                 pure (nValue + rhs))
                                   forestM arg5)
                     arg2 <- M.lazy
                               (do arg3 <- M.lazy
                                             (do arg8 <- M.lazy
                                                           (do arg10 <- M.lazy (pure 2)
                                                               nValue' <- M.force n
                                                               rhs' <- M.force arg10
                                                               pure (nValue' + rhs'))
                                                 forestM arg8)
                                   arg4 <- M.lazy (pure L.NilT)
                                   pure (L.ConsT arg3 arg4))
                     pure (L.ConsT arg1 arg2))
       pure (InT arg0)

firstListedM ::
  forall (a :: Type). M.Th (HigherT L.ListT a) -> M.MemoristM a
firstListedM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         HigherT field0
           -> do matched'' <- M.force field0
                 case matched'' of
                   L.ConsT x _ -> M.force x
                   _ -> error "source pattern match failure"

wrapM ::
  forall (a :: Type). M.Th a -> M.MemoristM (HigherT L.ListT a)
wrapM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy (pure L.NilT)
                     pure (L.ConsT x arg2))
       pure (HigherT arg0)

listedM :: forall (a :: Type). M.Th a -> M.MemoristM a
listedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (wrapM x)
       firstListedM arg0

isEmptyM :: M.Th (FixT L.ListT) -> M.MemoristM Bool
isEmptyM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         InT field0
           -> do matched'' <- M.force field0
                 case matched'' of
                   L.NilT -> (pure True :: M.MemoristM Bool)
                   L.ConsT _ _ -> (pure False :: M.MemoristM Bool)

rollM ::
  forall (f :: Type -> Type).
  M.Th (f (FixT f)) -> M.MemoristM (FixT f)
rollM x
  = do (M.tick :: M.MemoristM ())
       pure (InT x)

observedM :: forall (p :: Type). Num p => M.Th p -> M.MemoristM p
observedM x
  = do (M.tick :: M.MemoristM ())
       condition <- do arg0 <- M.lazy
                                 (do arg1 <- M.lazy
                                               (pure L.NilT :: M.MemoristM (L.ListT (FixT L.ListT)))
                                     rollM arg1)
                       isEmptyM arg0
       if condition then M.force x else pure 0
