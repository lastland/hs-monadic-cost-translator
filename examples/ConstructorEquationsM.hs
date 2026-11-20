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

module ConstructorEquationsM where

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

-- ==== Module-specific: source datatypes and conversions from ConstructorEquations ====

-- ==== Module-specific: translated bindings from ConstructorEquations ====

guardedVariableM ::
  M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
guardedVariableM flag xs
  = do (M.tick :: M.MemoristM ())
       (let
          ignoreList :: M.Th (L.ListT Int) -> M.MemoristM Int
          ignoreList arg0
            = do (M.tick :: M.MemoristM ())
                 pure (7 :: Int) in
        let
          pick :: M.Th (L.ListT Int) -> M.MemoristM Int
          pick whole
            = do (M.tick :: M.MemoristM ())
                 condition <- (M.force flag :: M.MemoristM Bool)
                 if condition then
                     ignoreList (whole :: M.Th (L.ListT Int))
                 else
                     do matched' <- M.force whole
                        case matched' of
                          L.NilT -> pure (0 :: Int)
                          L.ConsT y _ -> (M.force y :: M.MemoristM Int)
        in pick (xs :: M.Th (L.ListT Int)))

guardedWildcardM ::
  M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
guardedWildcardM flag xs
  = do (M.tick :: M.MemoristM ())
       (let
          pick :: M.Th (L.ListT Int) -> M.MemoristM Int
          pick arg0
            = do (M.tick :: M.MemoristM ())
                 condition <- (M.force flag :: M.MemoristM Bool)
                 if condition then
                     pure (7 :: Int)
                 else
                     do matched' <- M.force arg0
                        case matched' of
                          L.NilT -> pure (0 :: Int)
                          L.ConsT y _ -> (M.force y :: M.MemoristM Int)
        in pick (xs :: M.Th (L.ListT Int)))

headValueM :: M.Th (L.ListT Int) -> M.MemoristM Int
headValueM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT -> pure (0 :: Int)
         L.ConsT y _ -> (M.force y :: M.MemoristM Int)

variableM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
variableM arg0 arg1
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg1
       case matched' of
         L.NilT -> pure (0 :: Int)
         _ -> do condition <- (M.force arg0 :: M.MemoristM Bool)
                 if condition then
                     headValueM (arg1 :: M.Th (L.ListT Int))
                 else
                     do matched'' <- M.force arg1
                        case matched'' of
                          L.ConsT y _ -> (M.force y :: M.MemoristM Int)
                          _ -> error "source pattern match failure"

wildcardM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
wildcardM flag arg0
  = do (M.tick :: M.MemoristM ())
       condition <- (M.force flag :: M.MemoristM Bool)
       if condition then
           pure (7 :: Int)
       else
           do matched' <- M.force arg0
              case matched' of
                L.NilT -> pure (0 :: Int)
                L.ConsT y _ -> (M.force y :: M.MemoristM Int)

negativeM :: M.Th Int -> M.MemoristM Bool
negativeM x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (pure (0 :: Int))
       xValue <- M.force (x :: M.Th Int)
       rhs <- M.force arg1
       pure (xValue < rhs)

positiveM :: M.Th Int -> M.MemoristM Bool
positiveM x
  = do (M.tick :: M.MemoristM ())
       arg1 <- M.lazy (pure (0 :: Int))
       xValue <- M.force (x :: M.Th Int)
       rhs <- M.force arg1
       pure (xValue > rhs)

chooseM :: M.Th (L.ListT Int) -> M.MemoristM Int
chooseM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT -> pure (0 :: Int)
         L.ConsT y _
           -> do condition <- positiveM (y :: M.Th Int)
                 if condition then
                     (M.force y :: M.MemoristM Int)
                 else
                     pure (0 :: Int)

orderedM :: M.Th Int -> M.MemoristM Int
orderedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy
                               (pure (error "source undefined") :: M.MemoristM (L.ListT Int))
                     pure (L.ConsT (x :: M.Th Int) arg2))
       chooseM arg0

repeatedM :: M.Th (L.ListT Int) -> M.MemoristM Int
repeatedM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT -> pure (0 :: Int)
         L.ConsT y _
           -> do condition <- positiveM (y :: M.Th Int)
                 if condition then
                     (M.force y :: M.MemoristM Int)
                 else
                     do condition <- negativeM (y :: M.Th Int)
                        if condition then
                            do arg2 <- M.lazy (pure (0 :: Int))
                               lhs <- M.force arg2
                               yValue <- M.force (y :: M.Th Int)
                               pure (lhs - yValue)
                        else
                            pure (0 :: Int)

fallthroughM :: M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM Int
fallthroughM n arg0
  = do (M.tick :: M.MemoristM ())
       let nextEquation1 = (M.force n :: M.MemoristM Int)
       matched' <- M.force arg0
       case matched' of
         L.NilT
           -> do condition <- positiveM (n :: M.Th Int)
                 if condition then (M.force n :: M.MemoristM Int) else nextEquation1
         L.ConsT y _
           -> do condition <- positiveM (y :: M.Th Int)
                 if condition then (M.force y :: M.MemoristM Int) else nextEquation1
