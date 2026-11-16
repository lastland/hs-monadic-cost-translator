{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Independent equations for the observed dictionary specializations. The
-- caches model source sharing, not generated names or structural type keys.
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data Bindings = Bindings
    { intCache :: M.Th Int
    , integerCache :: M.Th Integer
    , emptyCache :: forall a. M.Th (L.ListT a)
    }

initialize :: M.MemoristM Bindings
initialize = do
    int <- M.lazy (M.lazy (pure 5) >>= workValue)
    integer <- M.lazy (M.lazy (pure 5) >>= workValue)
    anchor <- M.lazy (pure ())
    let emptyThunk :: forall a. M.Th (L.ListT a)
        emptyThunk = atAnchor anchor $ do
            one <- M.lazy (pure (1 :: Int))
            emptyValue one
    pure (Bindings int integer emptyThunk)

-- A reserved ordinary name supports one unconstrained thunk at every element
-- type. No unsafe cast or generated sharing helper is needed.
atAnchor :: M.Th () -> M.MemoristM a -> M.Th a
atAnchor anchor action = M.value (M.eval (M.lazy action) (M.tname anchor))

workValue :: Num a => M.Th a -> M.MemoristM a
workValue x = do
    M.tick
    value <- M.force x
    pure (value + value)

workM :: Num a => Bindings -> M.Th a -> M.MemoristM a
workM _ = workValue

plainM, parenthesizedM, annotatedM, asPatternM, guardAliasM,
    guardParenthesizedM, guardAnnotatedM, guardAsM
    :: Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
plainM _ x = M.tick >> pure (M.PairT x x)
parenthesizedM _ x = M.tick >> pure (M.PairT x x)
annotatedM _ x = M.tick >> pure (M.PairT x x)
asPatternM _ x = M.tick >> pure (M.PairT x x)
guardAliasM _ x = M.tick >> pure (M.PairT x x)
guardParenthesizedM _ x = M.tick >> pure (M.PairT x x)
guardAnnotatedM _ x = M.tick >> pure (M.PairT x x)
guardAsM _ x = M.tick >> pure (M.PairT x x)

computedCaseM, computedGuardM, localM :: Num a
    => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
computedCaseM _ = computedPair
computedGuardM _ = computedPair
localM _ = computedPair

computedPair :: Num a => M.Th a -> M.MemoristM (M.PairT a a)
computedPair x = do
    M.tick
    shared <- M.lazy (workValue x)
    pure (M.PairT shared shared)

sourceLetM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
sourceLetM _ x = do
    M.tick
    shared <- M.lazy (M.force x)
    pure (M.PairT shared shared)

badM :: Bindings -> M.Th a -> M.MemoristM a
badM _ _ = M.tick >> error "unused matching subject evaluated"

ignoredCaseM, ignoredGuardM :: Bindings -> M.Th a -> M.MemoristM a
ignoredCaseM = ignored
ignoredGuardM = ignored

ignored :: Bindings -> M.Th a -> M.MemoristM a
ignored bindings x = do
    M.tick
    _ <- M.lazy (badM bindings x)
    M.force x

suspendedFailureM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
suspendedFailureM bindings x = do
    M.tick
    failed <- M.lazy (badM bindings x)
    pure (M.PairT x failed)

sameCaseM, sameGuardM :: Bindings -> M.Th t -> M.MemoristM (M.PairT Int Int)
sameCaseM bindings _ = M.tick >> pure (M.PairT (intCache bindings) (intCache bindings))
sameGuardM bindings _ = M.tick >> pure (M.PairT (intCache bindings) (intCache bindings))

differentM :: Bindings -> M.Th t -> M.MemoristM (M.PairT Int Integer)
differentM bindings _ = M.tick >> pure (M.PairT (intCache bindings) (integerCache bindings))

localDictionariesM :: Bindings -> M.Th t
    -> M.MemoristM (M.PairT Int (M.PairT Int Integer))
localDictionariesM _ _ = do
    M.tick
    int <- M.lazy (M.lazy (pure 5) >>= workValue)
    integer <- M.lazy (M.lazy (pure 5) >>= workValue)
    rest <- M.lazy (pure (M.PairT int integer))
    pure (M.PairT int rest)

emptyValue :: M.Th a -> M.MemoristM (L.ListT b)
emptyValue _ = M.tick >> pure L.NilT

emptyM :: Bindings -> M.Th a -> M.MemoristM (L.ListT b)
emptyM _ = emptyValue

polymorphicM :: Bindings -> M.Th t
    -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
polymorphicM bindings _ = M.tick >> pure (M.PairT (emptyCache bindings) (emptyCache bindings))

localPolymorphicM :: Bindings -> M.Th t
    -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolymorphicM _ x = do
    M.tick
    anchor <- M.lazy (pure ())
    let shared :: forall a. M.Th (L.ListT a)
        shared = atAnchor anchor (emptyValue x)
    pure (M.PairT shared shared)
