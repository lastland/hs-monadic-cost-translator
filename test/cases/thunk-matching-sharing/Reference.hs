{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Handwritten call-by-need semantics for Example.hs. The cache is specialized
-- to the two numeric dictionaries observed by Case, not keyed by emitted names.
module Generated
    ( polyCaseM, polyGuardM, localPolyCaseM, localPolyGuardM, sameCaseM
    , sameGuardM, differentM, scopedCaseM, scopedGuardM, localScopedM
    , wideBoolM, wideIntM, wideIntegerM, ignoredConcreteM, ignoredScopedM
    , ignoredDefaultM, ignoredLocalScopedM, defaultedCaseM, defaultedGuardM
    , bottomCaseM, bottomGuardM, computedBottomCaseM, computedBottomGuardM
    ) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data Bindings = Bindings
    { emptyCache :: forall a. M.Th (L.ListT a)
    , intCache :: M.Th Int
    , integerCache :: M.Th Integer
    , wideIntCache :: forall a. M.Th (M.PairT Int (L.ListT a))
    , wideIntegerCache :: forall a. M.Th (M.PairT Integer (L.ListT a))
    }

initialize :: M.MemoristM Bindings
initialize = do
    emptyAnchor <- M.lazy (pure ())
    int <- M.lazy $ M.lazy (pure (5 :: Int)) >>= workValue
    integer <- M.lazy $ M.lazy (pure (5 :: Integer)) >>= workValue
    wideIntAnchor <- M.lazy (pure ())
    wideIntegerAnchor <- M.lazy (pure ())
    let empty :: forall a. M.Th (L.ListT a)
        empty = atAnchor emptyAnchor $ do
            argument <- M.lazy (pure (1 :: Int))
            emptyValue argument
        wideInt :: forall a. M.Th (M.PairT Int (L.ListT a))
        wideInt = atAnchor wideIntAnchor wideValue
        wideInteger :: forall a. M.Th (M.PairT Integer (L.ListT a))
        wideInteger = atAnchor wideIntegerAnchor wideValue
    pure (Bindings empty int integer wideInt wideInteger)

-- Suspending the complete initialization keeps all global and dictionary-cache
-- names beneath negative ancestry, disjoint from ordinary caller allocation.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

-- Reserve a name with lazy, then run the ordinary runtime allocator at that
-- name. Let-generalization retains one thunk identity at every element type.
atAnchor :: M.Th () -> M.MemoristM a -> M.Th a
atAnchor anchor action = M.value (M.eval (M.lazy action) (M.tname anchor))

emptyValue :: M.Th a -> M.MemoristM (L.ListT b)
emptyValue _ = M.tick >> pure L.NilT

workValue :: Num a => M.Th a -> M.MemoristM a
workValue argument = do
    M.tick
    left <- M.force argument
    right <- M.force argument
    pure (left + right)

wideValue :: Num a => M.MemoristM (M.PairT a (L.ListT b))
wideValue = do
    numeric <- M.lazy $ M.lazy (pure 5) >>= workValue
    empty <- M.lazy (pure L.NilT)
    pure (M.PairT numeric empty)

topEmpty :: Bindings -> M.Th (L.ListT a)
topEmpty bindings = emptyCache bindings

polyCaseM :: M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Int))
polyCaseM = polyCaseWith globalBindings

polyCaseWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Int))
polyCaseWith bindings _ = do
    M.tick
    let matched = topEmpty bindings
    pure (M.PairT matched matched)

polyGuardM :: M.Th a -> M.MemoristM (M.PairT (L.ListT Bool) (L.ListT Bool))
polyGuardM = polyGuardWith globalBindings

polyGuardWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Bool) (L.ListT Bool))
polyGuardWith bindings _ = do
    M.tick
    let matched = topEmpty bindings
    pure (M.PairT matched matched)

localPolyCaseM :: M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolyCaseM argument = do
    M.tick
    anchor <- M.lazy (pure ())
    let local :: forall b. M.Th (L.ListT b)
        local = atAnchor anchor (emptyValue argument)
    pure (M.PairT local local)

localPolyGuardM :: M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolyGuardM argument = do
    M.tick
    anchor <- M.lazy (pure ())
    let local :: forall b. M.Th (L.ListT b)
        local = atAnchor anchor (emptyValue argument)
    pure (M.PairT local local)

class Num a => NumericCache a where
    sharedNumeric :: Bindings -> M.Th a

instance NumericCache Int where
    sharedNumeric = intCache

instance NumericCache Integer where
    sharedNumeric = integerCache

sameCaseM :: M.Th a -> M.MemoristM (M.PairT Int Int)
sameCaseM = sameCaseWith globalBindings

sameCaseWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameCaseWith bindings _ = do
    M.tick
    let matched = intCache bindings
    pure (M.PairT matched matched)

sameGuardM :: M.Th a -> M.MemoristM (M.PairT Int Int)
sameGuardM = sameGuardWith globalBindings

sameGuardWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameGuardWith bindings _ = do
    M.tick
    let matched = intCache bindings
    pure (M.PairT matched matched)

differentM :: M.Th a -> M.MemoristM (M.PairT Int Integer)
differentM = differentWith globalBindings

differentWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Integer)
differentWith bindings _ = do
    M.tick
    pure (M.PairT (intCache bindings) (integerCache bindings))

scopedCaseM :: NumericCache a => M.Th a -> M.MemoristM (M.PairT a a)
scopedCaseM = scopedCaseWith globalBindings

scopedCaseWith :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
scopedCaseWith bindings _ = do
    M.tick
    let matched = sharedNumeric bindings
    pure (M.PairT matched matched)

scopedGuardM :: NumericCache a => M.Th a -> M.MemoristM (M.PairT a a)
scopedGuardM = scopedGuardWith globalBindings

scopedGuardWith :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
scopedGuardWith bindings _ = do
    M.tick
    let matched = sharedNumeric bindings
    pure (M.PairT matched matched)

localScopedM :: NumericCache a => M.Th a -> M.MemoristM (M.PairT a a)
localScopedM = localScopedWith globalBindings

localScopedWith :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
localScopedWith bindings argument = do
    M.tick
    let go :: NumericCache b => M.Th b -> M.MemoristM (M.PairT b b)
        go _ = do
            M.tick
            let matched = sharedNumeric bindings
            pure (M.PairT matched matched)
    go argument

-- The second parameter of wide is unconstrained. Its empty list can therefore
-- share the same enclosing thunk at Bool and Int; the Num dictionary cannot.
wideBoolM :: M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Bool)) (M.PairT Int (L.ListT Bool)))
wideBoolM = wideBoolWith globalBindings

wideBoolWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Bool)) (M.PairT Int (L.ListT Bool)))
wideBoolWith bindings _ = do
    M.tick
    let matched = wideIntCache bindings
    pure (M.PairT matched matched)

wideIntM :: M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Int)) (M.PairT Int (L.ListT Int)))
wideIntM = wideIntWith globalBindings

wideIntWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Int)) (M.PairT Int (L.ListT Int)))
wideIntWith bindings _ = do
    M.tick
    let matched = wideIntCache bindings
    pure (M.PairT matched matched)

wideIntegerM :: M.Th a -> M.MemoristM (M.PairT (M.PairT Integer (L.ListT Bool)) (M.PairT Integer (L.ListT Bool)))
wideIntegerM = wideIntegerWith globalBindings

wideIntegerWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Integer (L.ListT Bool)) (M.PairT Integer (L.ListT Bool)))
wideIntegerWith bindings _ = do
    M.tick
    let matched = wideIntegerCache bindings
    pure (M.PairT matched matched)

failingValue :: M.MemoristM a
failingValue = error "ignored shared value evaluated"

ignoredConcreteM :: M.Th a -> M.MemoristM a
ignoredConcreteM argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM Int)
    M.force argument

ignoredScopedM :: forall a. Num a => M.Th a -> M.MemoristM a
ignoredScopedM argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM a)
    M.force argument

ignoredDefaultM :: M.Th a -> M.MemoristM a
ignoredDefaultM argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM Integer)
    M.force argument

ignoredLocalScopedM :: NumericCache a => M.Th a -> M.MemoristM a
ignoredLocalScopedM argument = do
    M.tick
    let go :: forall b. Num b => M.Th b -> M.MemoristM b
        go value = do
            M.tick
            _ <- M.lazy (failingValue :: M.MemoristM b)
            M.force value
    go argument

defaultedCaseM :: M.Th a -> M.MemoristM a
defaultedCaseM = defaultedCaseWith globalBindings

defaultedCaseWith :: Bindings -> M.Th a -> M.MemoristM a
defaultedCaseWith bindings argument = do
    M.tick
    left <- M.force (integerCache bindings)
    right <- M.force (integerCache bindings)
    if left + right > 0 then M.force argument else M.force argument

defaultedGuardM :: M.Th a -> M.MemoristM a
defaultedGuardM = defaultedGuardWith globalBindings

defaultedGuardWith :: Bindings -> M.Th a -> M.MemoristM a
defaultedGuardWith bindings argument = do
    M.tick
    left <- M.force (integerCache bindings)
    right <- M.force (integerCache bindings)
    if left + right > 0 then M.force argument else M.force argument

bottomCaseM :: M.Th a -> M.MemoristM (M.PairT a Int)
bottomCaseM argument = do
    M.tick
    local <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument local)

bottomGuardM :: M.Th a -> M.MemoristM (M.PairT a Int)
bottomGuardM argument = do
    M.tick
    local <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument local)

computedBottomCaseM :: M.Th a -> M.MemoristM (M.PairT a Int)
computedBottomCaseM argument = do
    M.tick
    matched <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument matched)

computedBottomGuardM :: M.Th a -> M.MemoristM (M.PairT a Int)
computedBottomGuardM argument = do
    M.tick
    matched <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument matched)
