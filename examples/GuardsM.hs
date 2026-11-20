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

module GuardsM where

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

-- ==== Module-specific: source datatypes and conversions from Guards ====

-- ==== Module-specific: translated bindings from Guards ====

keepM :: M.Th Int -> M.MemoristM Int
keepM x
  = do (M.tick :: M.MemoristM ())
       let unusedbody :: forall (a' :: Type). M.MemoristM a'
           unusedbody = pure (error "unused")
       unusedanchor <- M.lazy unusedbody
       let unused = mapThunk Void.absurd unusedanchor
       xValue <- M.force (x :: M.Th Int)
       (let arg1 = pure xValue
        in
          if xValue < (xValue + (1 :: Int)) then
              arg1
          else
              do undefinedValue <- do arg2 <- M.lazy
                                                (pure (error "source undefined") ::
                                                   M.MemoristM Bool)
                                      M.force arg2
                 if undefinedValue then
                     arg1
                 else
                     (M.force unused :: M.MemoristM Int))

pickM :: forall (a :: Type). Num a => M.Th Char -> M.MemoristM a
pickM c
  = do (M.tick :: M.MemoristM ())
       cValue <- M.force (c :: M.Th Char)
       if cValue == 'z' then pure 10 else pure 20

namedM ::
  forall (a :: Type). Num a => M.Th (L.ListT Char) -> M.MemoristM a
namedM arg0
  = do (M.tick :: M.MemoristM ())
       let nextEquation0 = pure 4
       matched' <- M.force arg0
       case matched' of
         L.ConsT field0 field1
           -> do matched'' <- M.force field0
                 if matched'' == 'a' then
                     do matched''' <- M.force field1
                        case matched''' of
                          L.ConsT field2 field3
                            -> do matched'''' <- M.force field2
                                  if matched'''' == 'b' then
                                      do matched''''' <- M.force field3
                                         case matched''''' of
                                           L.NilT -> pure 3
                                           _ -> nextEquation0
                                  else
                                      nextEquation0
                          _ -> nextEquation0
                 else
                     nextEquation0
         _ -> nextEquation0

tagM :: forall (a :: Type). Num a => M.Th Char -> M.MemoristM a
tagM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       if matched' == 'a' then pure 1 else pure 2

observedM ::
  forall (a :: Type) (p :: Type). Num a => M.Th p -> M.MemoristM a
observedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy
                               (do arg4 <- M.lazy (pure 'a')
                                   tagM arg4)
                     arg3 <- M.lazy
                               (do arg5 <- M.lazy
                                             (do arg6 <- M.lazy (pure 'a')
                                                 arg7 <- M.lazy
                                                           (do arg8 <- M.lazy (pure 'b')
                                                               arg9 <- M.lazy (pure L.NilT)
                                                               pure (L.ConsT arg8 arg9))
                                                 pure (L.ConsT arg6 arg7))
                                   namedM arg5)
                     lhs' <- M.force arg2
                     rhs' <- M.force arg3
                     pure (lhs' + rhs'))
       arg1 <- M.lazy
                 (do arg10 <- M.lazy (pure 'z')
                     pickM arg10)
       lhs <- M.force arg0
       rhs <- M.force arg1
       pure (lhs + rhs)

fallbackM ::
  forall (a :: Type).
  (Eq a, Scalar a, Num a) => M.Th a -> M.MemoristM a
fallbackM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       if matched' == 0 then
           pure (error "source undefined")
       else
           M.force arg0

numericM ::
  forall (a :: Type).
  (Eq a, Scalar a, Num a) => M.Th a -> M.MemoristM a
numericM x
  = do (M.tick :: M.MemoristM ())
       fallbackM x

guardedM ::
  forall (a :: Type).
  (Ord a, Scalar a, Num a) => M.Th a -> M.MemoristM a
guardedM x
  = do (M.tick :: M.MemoristM ())
       caseThunk0 <- M.lazy
                       (do arg1 <- M.lazy (pure (error "source undefined"))
                           pure (L.ConsT x arg1))
       let nextEquation0 = pure (error "source undefined")
       matched' <- M.force caseThunk0
       case matched' of
         L.ConsT h _
           -> do zanchor <- M.lazy
                              (do hValue <- M.force h
                                  hValue' <- M.force h
                                  pure (hValue + hValue'))
                 let z = zanchor
                 arg5 <- M.lazy (pure 0)
                 zValue <- M.force z
                 rhs <- M.force arg5
                 if zValue > rhs then M.force z else nextEquation0
         _ -> nextEquation0

chooseM ::
  forall (a :: Type).
  (Ord a, Scalar a, Num a) => M.Th (L.ListT a) -> M.MemoristM a
chooseM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT -> pure (error "source undefined")
         L.ConsT y _
           -> do arg1 <- M.lazy (pure 0)
                 yValue <- M.force y
                 rhs <- M.force arg1
                 if yValue < rhs then pure (error "source undefined") else M.force y

orderedM ::
  forall (a :: Type).
  (Ord a, Scalar a, Num a) => M.Th a -> M.MemoristM a
orderedM x
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy
                 (do arg2 <- M.lazy (pure (error "source undefined"))
                     pure (L.ConsT x arg2))
       chooseM arg0
