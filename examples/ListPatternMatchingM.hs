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

module ListPatternMatchingM where

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

-- ==== Module-specific: source datatypes and conversions from ListPatternMatching ====

-- ==== Module-specific: translated bindings from ListPatternMatching ====

takeM ::
  forall (a :: Type).
  M.Th Int -> M.Th (L.ListT a) -> M.MemoristM (L.ListT a)
takeM n xs
  = do (M.tick :: M.MemoristM ())
       arg0 <- M.lazy (pure (0 :: Int))
       lhs <- M.force arg0
       nValue <- M.force (n :: M.Th Int)
       if lhs < nValue then
           do matched' <- M.force xs
              case matched' of
                L.NilT -> pure L.NilT
                L.ConsT y ys
                  -> do arg3 <- M.lazy
                                  (do arg4 <- M.lazy
                                                (do arg7 <- M.lazy (pure (1 :: Int))
                                                    nValue' <- M.force (n :: M.Th Int)
                                                    rhs <- M.force arg7
                                                    pure (nValue' - rhs))
                                      takeM arg4 ys)
                        pure (L.ConsT y arg3)
       else
           pure L.NilT

zip'M ::
  forall (a :: Type) (b :: Type).
  M.Th (L.ListT a)
  -> M.Th (L.ListT b) -> M.MemoristM (L.ListT (M.PairT a b))
zip'M arg0 arg1
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT
           -> do matched'' <- M.force arg1
                 case matched'' of
                   L.NilT -> pure L.NilT
                   L.ConsT _ _ -> pure L.NilT
         L.ConsT a' as
           -> do matched'''''' <- M.force arg1
                 case matched'''''' of
                   L.NilT -> pure L.NilT
                   L.ConsT b' bs
                     -> do arg0' <- M.lazy (pure (M.PairT a' b'))
                           arg1' <- M.lazy (zip'M as bs)
                           pure (L.ConsT arg0' arg1')

zipM ::
  forall (a :: Type) (b :: Type).
  M.Th (L.ListT a)
  -> M.Th (L.ListT b) -> M.MemoristM (L.ListT (M.PairT a b))
zipM arg0 arg1
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.NilT -> pure L.NilT
         _ -> do matched'' <- M.force arg1
                 case matched'' of
                   L.NilT -> pure L.NilT
                   _ -> do matched''' <- M.force arg0
                           case matched''' of
                             L.ConsT a' as
                               -> do matched'''' <- M.force arg1
                                     case matched'''' of
                                       L.ConsT b' bs
                                         -> do arg0' <- M.lazy (pure (M.PairT a' b'))
                                               arg1' <- M.lazy (zipM as bs)
                                               pure (L.ConsT arg0' arg1')
                                       _ -> error "source pattern match failure"
                             _ -> error "source pattern match failure"

maybeHead'M ::
  forall (a :: Type). M.Th (L.ListT a) -> M.MemoristM (M.MaybeT a)
maybeHead'M arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.ConsT x _ -> pure (M.JustT x)
         L.NilT -> pure M.NothingT

maybeHeadM ::
  forall (a :: Type). M.Th (L.ListT a) -> M.MemoristM (M.MaybeT a)
maybeHeadM arg0
  = do (M.tick :: M.MemoristM ())
       matched' <- M.force arg0
       case matched' of
         L.ConsT x _ -> pure (M.JustT x)
         _ -> pure M.NothingT
