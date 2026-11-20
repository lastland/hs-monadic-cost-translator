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

module SharedM where

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

-- ==== Module-specific: source datatypes and conversions from Shared ====

-- ==== Module-specific: translated bindings from Shared ====

expensiveM ::
  forall (t :: Type).
  (Eq t, Scalar t, Num t) => M.Th t -> M.MemoristM t
expensiveM n
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
                            expensiveM arg6)
              arg5 <- M.lazy (pure 1)
              lhs <- M.force arg4
              rhs'' <- M.force arg5
              pure (lhs + rhs'')

sharedM ::
  forall (a :: Type). (Eq a, Scalar a, Num a) => M.MemoristM a
sharedM
  = do arg0 <- M.lazy (pure 18446744073709551616)
       expensiveM arg0

asIntM :: M.Th Int -> M.MemoristM Int
asIntM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (sharedM :: M.MemoristM Int)
       sharedValue <- M.force arg0
       xValue <- M.force (x :: M.Th Int)
       pure (sharedValue + xValue)

asIntegerM :: M.Th Integer -> M.MemoristM Integer
asIntegerM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (sharedM :: M.MemoristM Integer)
       sharedValue <- M.force arg0
       xValue <- M.force (x :: M.Th Integer)
       pure (sharedValue + xValue)

doubleM :: forall (a :: Type). Num a => M.Th a -> M.MemoristM a
doubleM x
  = do (M.tick :: M.MemoristM ())
       xValue <- M.force x
       xValue' <- M.force x
       pure (xValue + xValue')

seedM :: M.MemoristM Int
seedM
  = do arg0 <- M.lazy (pure (5 :: Int))
       doubleM arg0

firstM :: M.Th Int -> M.MemoristM Int
firstM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (seedM :: M.MemoristM Int)
       seedValue <- M.force arg0
       xValue <- M.force (x :: M.Th Int)
       pure (seedValue + xValue)

secondM :: M.Th Int -> M.MemoristM Int
secondM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (seedM :: M.MemoristM Int)
       seedValue <- M.force arg0
       xValue <- M.force (x :: M.Th Int)
       pure (seedValue + xValue)
