{-# LANGUAGE
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    RankNTypes,
    KindSignatures,
    NoMonoLocalBinds #-}
-- Runtime-owned types require local instances; generic dictionaries retain intentional constraints.
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module ScopedAnnotationsM where

import Data.Kind
import Prelude

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

-- ==== Module-specific: source datatypes and conversions from ScopedAnnotations ====

-- ==== Module-specific: translated bindings from ScopedAnnotations ====

workM ::
  forall (a :: Type).
  (Num a, Eq a, Scalar a) => M.Th a -> M.MemoristM a
workM n
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (pure 0)
       nValue <- M.force n
       rhs <- M.force arg1
       if nValue == rhs then
           do arg3 <- M.lazy (pure 1)
              nValue' <- M.force n
              rhs' <- M.force arg3
              pure (nValue' + rhs')
       else
           do arg4 <- M.lazy
                        (do arg6 <- M.lazy
                                      (do nValue'' <- M.force n
                                          nValue''' <- M.force n
                                          pure (nValue'' - nValue'''))
                            workM arg6)
              arg5 <- M.lazy (pure 1)
              lhs <- M.force arg4
              rhs'' <- M.force arg5
              pure (lhs + rhs'')

sharedM ::
  forall (a :: Type). (Num a, Eq a, Scalar a) => M.MemoristM a
sharedM
  = do arg0 <- M.lazy (pure 18446744073709551616)
       workM arg0

caseFactoryM ::
  forall (a :: Type).
  (Num a, Eq a, Scalar a) => M.Th a -> M.MemoristM Int
caseFactoryM arg0
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy (sharedM :: M.MemoristM a)
       let inner :: forall (a :: Type). M.Th a -> M.MemoristM Int
           inner arg1
             = do (M.tick :: M.MemoristM ())
                  arg1' <- M.lazy (pure 1)
                  yValue <- M.force caseThunk0
                  rhs <- M.force arg1'
                  if yValue == rhs then pure (1 :: Int) else pure (2 :: Int)
       arg2 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg2

guardFactoryM ::
  forall (a :: Type).
  (Num a, Eq a, Scalar a) => M.Th a -> M.MemoristM Int
guardFactoryM arg0
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy (sharedM :: M.MemoristM a)
       let inner :: forall (a :: Type). M.Th a -> M.MemoristM Int
           inner arg1
             = do (M.tick :: M.MemoristM ())
                  arg1' <- M.lazy (pure 1)
                  yValue <- M.force guardThunk0
                  rhs <- M.force arg1'
                  if yValue == rhs then pure (1 :: Int) else pure (2 :: Int)
       arg2 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg2

guardFactoryRenamedM ::
  forall (a :: Type).
  (Num a, Eq a, Scalar a) => M.Th a -> M.MemoristM Int
guardFactoryRenamedM arg0
  = do (M.tick :: M.MemoristM ())
       guardThunk0 <- M.lazy (sharedM :: M.MemoristM a)
       let inner :: forall (b :: Type). M.Th b -> M.MemoristM Int
           inner arg1
             = do (M.tick :: M.MemoristM ())
                  arg1' <- M.lazy (pure 1)
                  yValue <- M.force guardThunk0
                  rhs <- M.force arg1'
                  if yValue == rhs then pure (1 :: Int) else pure (2 :: Int)
       arg2 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg2

ignoreM :: forall (t :: Type). M.Th t -> M.MemoristM Int
ignoreM arg0
  = do (M.tick :: M.MemoristM ())
       pure (1 :: Int)

caseParameterM :: forall (a :: Type). M.Th a -> M.MemoristM Int
caseParameterM x
  = do (M.tick :: M.MemoristM ())
       let caseThunk0 = (x :: M.Th a)
       let inner :: forall (a :: Type). M.Th a -> M.MemoristM Int
           inner arg0
             = do (M.tick :: M.MemoristM ())
                  ignoreM caseThunk0
       arg1 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg1

guardParameterM :: forall (a :: Type). M.Th a -> M.MemoristM Int
guardParameterM x
  = do (M.tick :: M.MemoristM ())
       let guardThunk0 = (x :: M.Th a)
       let inner :: forall (a :: Type). M.Th a -> M.MemoristM Int
           inner arg0
             = do (M.tick :: M.MemoristM ())
                  ignoreM guardThunk0
       arg1 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg1

caseParameterRenamedM ::
  forall (a :: Type). M.Th a -> M.MemoristM Int
caseParameterRenamedM x
  = do (M.tick :: M.MemoristM ())
       let caseThunk0 = (x :: M.Th a)
       let inner :: forall (b :: Type). M.Th b -> M.MemoristM Int
           inner arg0
             = do (M.tick :: M.MemoristM ())
                  ignoreM caseThunk0
       arg1 <- M.lazy (pure True :: M.MemoristM Bool)
       inner arg1
