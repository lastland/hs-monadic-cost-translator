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

module ListsM where

import Data.Kind
import Prelude

import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
import qualified Memorist.Example.ListInf as MI
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

-- ==== Module-specific: source datatypes and conversions from Lists ====

-- ==== Module-specific: translated bindings from Lists ====

takeM ::
  forall (a :: Type).
  M.Th Int -> M.Th (L.ListT a) -> M.MemoristM (L.ListT a)
takeM n arg0
  = do (M.tick :: M.MemoristM ())
       nValue <- M.force (n :: M.Th Int)
       if nValue <= (0 :: Int) then
           pure L.NilT
       else
           do matched' <- M.force arg0
              case matched' of
                L.NilT -> pure L.NilT
                L.ConsT x xs
                  -> do arg4 <- M.lazy (pure (nValue - (1 :: Int)))
                        arg3 <- M.lazy (takeM arg4 xs)
                        pure (L.ConsT x arg3)

repeatM :: forall (a :: Type). M.Th a -> M.MemoristM (L.ListT a)
repeatM x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (repeatM x)
       pure (L.ConsT x arg1)

replicateM ::
  forall (a :: Type). M.Th Int -> M.Th a -> M.MemoristM (L.ListT a)
replicateM n x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (repeatM x)
       takeM (n :: M.Th Int) arg1

secondM :: forall (a :: Type). M.Th (L.ListT a) -> M.MemoristM a
secondM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.ConsT _ field0
           -> do matched'' <- M.force field0
                 case matched'' of
                   L.ConsT x _ -> M.force x
                   _ -> error "source pattern match failure"
         _ -> error "source pattern match failure"

streamM ::
  forall (t :: Type). Num t => M.Th t -> M.MemoristM (L.ListT t)
streamM n
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy
                 (do arg2 <- M.lazy
                               (do arg4 <- M.lazy (pure 1)
                                   nValue <- M.force n
                                   rhs <- M.force arg4
                                   pure (nValue + rhs))
                     streamM arg2)
       pure (L.ConsT n arg1)

observeM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
observeM n
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (streamM n)
       secondM arg0

restM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
restM n
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy (streamM n)
                     ML.tailM arg1)
       ML.headM arg0

alwaysM :: forall (p :: Type). M.Th p -> M.MemoristM Bool
alwaysM x
  = do (M.tick :: M.MemoristM ())
       (pure True :: M.MemoristM Bool)

filteredM :: forall (a :: Type). M.Th a -> M.MemoristM a
filteredM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy (pure alwaysM)
                     arg2 <- M.lazy
                               (do arg4 <- M.lazy (pure (error "source undefined"))
                                   pure (L.ConsT x arg4))
                     ML.filterM arg1 arg2)
       ML.headM arg0

condM :: forall (a :: Type). M.Th a -> M.MemoristM (L.ListT a)
condM n
  = do (M.tick :: M.MemoristM ())
       condition <- ((pure (error "source undefined") ::
                        M.MemoristM Bool) ::
                       M.MemoristM Bool)
       if condition then
           do arg1 <- M.lazy (pure L.NilT)
              pure (L.ConsT n arg1)
       else
           pure L.NilT

addM ::
  forall (a :: Type). Num a => M.Th a -> M.Th a -> M.MemoristM a
addM x y
  = do (M.tick :: M.MemoristM ())
       xValue <- M.force x
       yValue <- M.force y
       pure (xValue + yValue)

foldedM ::
  forall (t :: Type -> Type) (b :: Type).
  (FoldableT t, Num b) => M.Th (t b) -> M.MemoristM b
foldedM xs
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (pure addM)
       arg1 <- M.lazy (pure 0)
       foldrT arg0 arg1 xs

summedFromLeftM ::
  forall (a :: Type). Num a => M.Th a -> M.MemoristM a
summedFromLeftM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (pure addM)
       arg1 <- M.lazy (pure 0)
       arg2 <- M.lazy
                 (do arg4 <- M.lazy
                               (do arg6 <- M.lazy (pure L.NilT)
                                   pure (L.ConsT x arg6))
                     pure (L.ConsT x arg4))
       foldlT arg0 arg1 arg2

zippedM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
zippedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy (pure addM)
                     arg2 <- M.lazy
                               (do arg5 <- M.lazy (pure (error "source undefined"))
                                   pure (L.ConsT x arg5))
                     arg3 <- M.lazy
                               (do arg7 <- M.lazy (pure (error "source undefined"))
                                   pure (L.ConsT x arg7))
                     ML.zipWithM arg1 arg2 arg3)
       ML.headM arg0

scannedM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
scannedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg1 <- M.lazy (pure addM)
                     arg3 <- M.lazy (condM x)
                     MI.scanlM arg1 x arg3)
       ML.headM arg0

bumpM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
bumpM x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (pure 1)
       xValue <- M.force x
       rhs <- M.force arg1
       pure (xValue + rhs)

mappedM ::
  forall (b :: Type).
  Num b => M.Th (L.ListT b) -> M.MemoristM (L.ListT b)
mappedM xs
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (pure bumpM)
       ML.mapMM arg0 xs
