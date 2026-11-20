{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    RankNTypes,
    KindSignatures,
    NoMonoLocalBinds #-}
-- Runtime-owned types require local instances; generic dictionaries retain intentional constraints.
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module ThunkMatchingM where

import Data.Kind
import Prelude

import qualified Data.Void as Void
import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- ==== Common: sharing and scalar support ====

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk convert (M.Th name payload annotation)
  = M.Th name (convert payload) annotation

mapListT :: (a -> b) -> L.ListT a -> L.ListT b
mapListT convert list
  = case list of
      L.NilT -> L.NilT
      L.ConsT headThunk tailThunk
        -> L.ConsT
             (mapThunk convert headThunk)
             (mapThunk (mapListT convert) tailThunk)

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

-- ==== Module-specific: source datatypes and conversions from ThunkMatching ====

-- ==== Module-specific: translated bindings from ThunkMatching ====

emptyM ::
  forall (a :: Type) (b :: Type). M.Th a -> M.MemoristM (L.ListT b)
emptyM arg0
  = do (M.tick :: M.MemoristM ())
       pure L.NilT

sharedEmptyM :: forall (a :: Type). M.MemoristM (L.ListT a)
sharedEmptyM
  = do arg0 <- M.lazy (pure (1 :: Int) :: M.MemoristM Int)
       emptyM arg0

polymorphicM ::
  forall (t :: Type).
  M.Th t -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
polymorphicM arg0
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy
                       ((sharedEmptyM :: M.MemoristM (L.ListT Int)) ::
                          M.MemoristM (L.ListT Int))
       caseThunk1 <- M.lazy
                       ((sharedEmptyM :: M.MemoristM (L.ListT Bool)) ::
                          M.MemoristM (L.ListT Bool))
       pure
         (M.PairT
            (caseThunk0 :: M.Th (L.ListT Int))
            (caseThunk1 :: M.Th (L.ListT Bool)))

localPolymorphicM ::
  forall (t :: Type).
  M.Th t -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolymorphicM x
  = do (M.tick :: M.MemoristM ())
       let valuesbody :: forall (b' :: Type). M.MemoristM (L.ListT b')
           valuesbody = emptyM x
       valuesanchor <- M.lazy valuesbody
       (let values = mapThunk (mapListT Void.absurd) valuesanchor
        in
          pure
            (M.PairT
               (((values :: M.Th (L.ListT Int)) :: M.Th (L.ListT Int)) ::
                  M.Th (L.ListT Int))
               (((values :: M.Th (L.ListT Bool)) :: M.Th (L.ListT Bool)) ::
                  M.Th (L.ListT Bool))))

badM :: forall (a :: Type). M.Th a -> M.MemoristM a
badM arg0
  = do (M.tick :: M.MemoristM ())
       pure (error "unused matching subject evaluated")

ignoredCaseM :: forall (a :: Type). M.Th a -> M.MemoristM a
ignoredCaseM x
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy (badM x)
       M.force x

ignoredGuardM :: forall (a :: Type). M.Th a -> M.MemoristM a
ignoredGuardM x
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy (badM x)
       M.force x

suspendedFailureM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
suspendedFailureM x
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy (badM x)
       pure (M.PairT x caseThunk0)

sourceLetM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
sourceLetM x
  = do (M.tick :: M.MemoristM ())
       boundanchor <- M.lazy (M.force x)
       (let bound = boundanchor in pure (M.PairT bound bound))

workM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
workM x
  = do (M.tick :: M.MemoristM ())
       xValue <- M.force x
       xValue' <- M.force x
       pure (xValue + xValue')

computedCaseM ::
  forall (a :: Type). Num a => M.Th a -> M.MemoristM (M.PairT a a)
computedCaseM x
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy (workM x)
       pure (M.PairT caseThunk0 caseThunk0)

computedGuardM ::
  forall (a :: Type). Num a => M.Th a -> M.MemoristM (M.PairT a a)
computedGuardM x
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy (workM x)
       pure (M.PairT guardThunk0 guardThunk0)

localM ::
  forall (a :: Type). Num a => M.Th a -> M.MemoristM (M.PairT a a)
localM x
  = do (M.tick :: M.MemoristM ())
       boundanchor <- M.lazy (workM x)
       (let bound = boundanchor in pure (M.PairT bound bound))

sharedM :: forall (a :: Type). Num a => M.MemoristM a
sharedM
  = do arg0 <- M.lazy (pure 5)
       workM arg0

sameCaseM ::
  forall (t :: Type). M.Th t -> M.MemoristM (M.PairT Int Int)
sameCaseM arg0
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy
                       ((sharedM :: M.MemoristM Int) :: M.MemoristM Int)
       arg0' <- M.lazy ((sharedM :: M.MemoristM Int) :: M.MemoristM Int)
       pure (M.PairT arg0' (caseThunk0 :: M.Th Int))

sameGuardM ::
  forall (t :: Type). M.Th t -> M.MemoristM (M.PairT Int Int)
sameGuardM arg0
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy
                        ((sharedM :: M.MemoristM Int) :: M.MemoristM Int)
       arg0' <- M.lazy ((sharedM :: M.MemoristM Int) :: M.MemoristM Int)
       pure (M.PairT arg0' (guardThunk0 :: M.Th Int))

differentM ::
  forall (t :: Type). M.Th t -> M.MemoristM (M.PairT Int Integer)
differentM arg0
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy
                        ((sharedM :: M.MemoristM Integer) :: M.MemoristM Integer)
       arg0' <- M.lazy ((sharedM :: M.MemoristM Int) :: M.MemoristM Int)
       pure (M.PairT arg0' (guardThunk0 :: M.Th Integer))

localDictionariesM ::
  forall (t :: Type).
  M.Th t -> M.MemoristM (M.PairT Int (M.PairT Int Integer))
localDictionariesM arg0
  = do (M.tick :: M.MemoristM ())
       let valuebody :: forall (a :: Type). Num a => M.MemoristM a
           valuebody
             = do arg0' <- M.lazy (pure 5)
                  workM arg0'
       value0 <- M.lazy (valuebody :: M.MemoristM Integer)
       value1 <- M.lazy (valuebody :: M.MemoristM Int)
       arg2 <- M.lazy
                 (pure
                    (M.PairT
                       (((value1 :: M.Th Int) :: M.Th Int) :: M.Th Int)
                       ((value0 :: M.Th Integer) :: M.Th Integer)))
       pure (M.PairT ((value1 :: M.Th Int) :: M.Th Int) arg2)

guardAnnotatedM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
guardAnnotatedM x
  = do (M.tick :: M.MemoristM ())
       (let guardThunk0 = (x :: M.Th a) in pure (M.PairT x guardThunk0))

annotatedM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
annotatedM x
  = do (M.tick :: M.MemoristM ())
       (let caseThunk0 = (x :: M.Th a) in pure (M.PairT x caseThunk0))

guardAsM :: forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
guardAsM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)

guardParenthesizedM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
guardParenthesizedM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)

guardAliasM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
guardAliasM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)

asPatternM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
asPatternM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)

parenthesizedM ::
  forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
parenthesizedM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)

plainM :: forall (a :: Type). M.Th a -> M.MemoristM (M.PairT a a)
plainM x
  = do (M.tick :: M.MemoristM ())
       pure (M.PairT x x)
