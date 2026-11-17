{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Independent equations for the observed dictionary specializations. The
-- caches model source sharing, not generated names or structural type keys.
module Generated
    ( workM, plainM, parenthesizedM, annotatedM, asPatternM, guardAliasM
    , guardParenthesizedM, guardAnnotatedM, guardAsM, computedCaseM
    , computedGuardM, localM, sourceLetM, badM, ignoredCaseM, ignoredGuardM
    , suspendedFailureM, sameCaseM, sameGuardM, differentM, localDictionariesM
    , emptyM, polymorphicM, localPolymorphicM
    ) where

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

-- Suspend the whole cache initialization so every global name has negative
-- ancestry and cannot collide with a caller's ordinary allocation.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

-- A reserved ordinary name supports one unconstrained thunk at every element
-- type. No unsafe cast or generated sharing helper is needed.
atAnchor :: M.Th () -> M.MemoristM a -> M.Th a
atAnchor anchor action = M.value (M.eval (M.lazy action) (M.tname anchor))

workValue :: Num a => M.Th a -> M.MemoristM a
workValue x = do
    M.tick
    value <- M.force x
    pure (value + value)

workM :: Num a => M.Th a -> M.MemoristM a
workM = workValue

plainM, parenthesizedM, annotatedM, asPatternM, guardAliasM,
    guardParenthesizedM, guardAnnotatedM, guardAsM
    :: M.Th a -> M.MemoristM (M.PairT a a)
plainM x = M.tick >> pure (M.PairT x x)
parenthesizedM x = M.tick >> pure (M.PairT x x)
annotatedM x = M.tick >> pure (M.PairT x x)
asPatternM x = M.tick >> pure (M.PairT x x)
guardAliasM x = M.tick >> pure (M.PairT x x)
guardParenthesizedM x = M.tick >> pure (M.PairT x x)
guardAnnotatedM x = M.tick >> pure (M.PairT x x)
guardAsM x = M.tick >> pure (M.PairT x x)

computedCaseM, computedGuardM, localM :: Num a
    => M.Th a -> M.MemoristM (M.PairT a a)
computedCaseM = computedPair
computedGuardM = computedPair
localM = computedPair

computedPair :: Num a => M.Th a -> M.MemoristM (M.PairT a a)
computedPair x = do
    M.tick
    shared <- M.lazy (workValue x)
    pure (M.PairT shared shared)

sourceLetM :: M.Th a -> M.MemoristM (M.PairT a a)
sourceLetM x = do
    M.tick
    shared <- M.lazy (M.force x)
    pure (M.PairT shared shared)

badM :: M.Th a -> M.MemoristM a
badM _ = M.tick >> error "unused matching subject evaluated"

ignoredCaseM, ignoredGuardM :: M.Th a -> M.MemoristM a
ignoredCaseM = ignored
ignoredGuardM = ignored

ignored :: M.Th a -> M.MemoristM a
ignored x = do
    M.tick
    _ <- M.lazy (badM x)
    M.force x

suspendedFailureM :: M.Th a -> M.MemoristM (M.PairT a a)
suspendedFailureM x = do
    M.tick
    failed <- M.lazy (badM x)
    pure (M.PairT x failed)

sameCaseM, sameGuardM :: M.Th t -> M.MemoristM (M.PairT Int Int)
sameCaseM = sameCaseWith globalBindings
sameGuardM = sameGuardWith globalBindings

sameCaseWith, sameGuardWith :: Bindings -> M.Th t -> M.MemoristM (M.PairT Int Int)
sameCaseWith bindings _ = M.tick >> pure (M.PairT (intCache bindings) (intCache bindings))
sameGuardWith bindings _ = M.tick >> pure (M.PairT (intCache bindings) (intCache bindings))

differentM :: M.Th t -> M.MemoristM (M.PairT Int Integer)
differentM = differentWith globalBindings

differentWith :: Bindings -> M.Th t -> M.MemoristM (M.PairT Int Integer)
differentWith bindings _ = M.tick >> pure (M.PairT (intCache bindings) (integerCache bindings))

localDictionariesM :: M.Th t
    -> M.MemoristM (M.PairT Int (M.PairT Int Integer))
localDictionariesM _ = do
    M.tick
    int <- M.lazy (M.lazy (pure 5) >>= workValue)
    integer <- M.lazy (M.lazy (pure 5) >>= workValue)
    rest <- M.lazy (pure (M.PairT int integer))
    pure (M.PairT int rest)

emptyValue :: M.Th a -> M.MemoristM (L.ListT b)
emptyValue _ = M.tick >> pure L.NilT

emptyM :: M.Th a -> M.MemoristM (L.ListT b)
emptyM = emptyValue

polymorphicM :: M.Th t
    -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
polymorphicM = polymorphicWith globalBindings

polymorphicWith :: Bindings -> M.Th t
    -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
polymorphicWith bindings _ = M.tick >> pure (M.PairT (emptyCache bindings) (emptyCache bindings))

localPolymorphicM :: M.Th t
    -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolymorphicM x = do
    M.tick
    anchor <- M.lazy (pure ())
    let shared :: forall a. M.Th (L.ListT a)
        shared = atAnchor anchor (emptyValue x)
    pure (M.PairT shared shared)
