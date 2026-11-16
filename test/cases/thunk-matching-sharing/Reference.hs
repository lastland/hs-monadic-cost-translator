{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Handwritten call-by-need semantics for Example.hs. The cache is specialized
-- to the two numeric dictionaries observed by Case, not keyed by emitted names.
module Generated where

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

polyCaseM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Int))
polyCaseM bindings _ = do
    M.tick
    let matched = topEmpty bindings
    pure (M.PairT matched matched)

polyGuardM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Bool) (L.ListT Bool))
polyGuardM bindings _ = do
    M.tick
    let matched = topEmpty bindings
    pure (M.PairT matched matched)

localPolyCaseM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolyCaseM _ argument = do
    M.tick
    anchor <- M.lazy (pure ())
    let local :: forall b. M.Th (L.ListT b)
        local = atAnchor anchor (emptyValue argument)
    pure (M.PairT local local)

localPolyGuardM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
localPolyGuardM _ argument = do
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

sameCaseM :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameCaseM bindings _ = do
    M.tick
    let matched = intCache bindings
    pure (M.PairT matched matched)

sameGuardM :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameGuardM bindings _ = do
    M.tick
    let matched = intCache bindings
    pure (M.PairT matched matched)

differentM :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Integer)
differentM bindings _ = do
    M.tick
    pure (M.PairT (intCache bindings) (integerCache bindings))

scopedCaseM :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
scopedCaseM bindings _ = do
    M.tick
    let matched = sharedNumeric bindings
    pure (M.PairT matched matched)

scopedGuardM :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
scopedGuardM bindings _ = do
    M.tick
    let matched = sharedNumeric bindings
    pure (M.PairT matched matched)

localScopedM :: NumericCache a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
localScopedM bindings argument = do
    M.tick
    let go :: NumericCache b => M.Th b -> M.MemoristM (M.PairT b b)
        go _ = do
            M.tick
            let matched = sharedNumeric bindings
            pure (M.PairT matched matched)
    go argument

-- The second parameter of wide is unconstrained. Its empty list can therefore
-- share the same enclosing thunk at Bool and Int; the Num dictionary cannot.
wideBoolM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Bool)) (M.PairT Int (L.ListT Bool)))
wideBoolM bindings _ = do
    M.tick
    let matched = wideIntCache bindings
    pure (M.PairT matched matched)

wideIntM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Int (L.ListT Int)) (M.PairT Int (L.ListT Int)))
wideIntM bindings _ = do
    M.tick
    let matched = wideIntCache bindings
    pure (M.PairT matched matched)

wideIntegerM :: Bindings -> M.Th a -> M.MemoristM (M.PairT (M.PairT Integer (L.ListT Bool)) (M.PairT Integer (L.ListT Bool)))
wideIntegerM bindings _ = do
    M.tick
    let matched = wideIntegerCache bindings
    pure (M.PairT matched matched)

failingValue :: M.MemoristM a
failingValue = error "ignored shared value evaluated"

ignoredConcreteM :: Bindings -> M.Th a -> M.MemoristM a
ignoredConcreteM _ argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM Int)
    M.force argument

ignoredScopedM :: forall a. Num a => Bindings -> M.Th a -> M.MemoristM a
ignoredScopedM _ argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM a)
    M.force argument

ignoredDefaultM :: Bindings -> M.Th a -> M.MemoristM a
ignoredDefaultM _ argument = do
    M.tick
    _ <- M.lazy (failingValue :: M.MemoristM Integer)
    M.force argument

ignoredLocalScopedM :: NumericCache a => Bindings -> M.Th a -> M.MemoristM a
ignoredLocalScopedM _ argument = do
    M.tick
    let go :: forall b. Num b => M.Th b -> M.MemoristM b
        go value = do
            M.tick
            _ <- M.lazy (failingValue :: M.MemoristM b)
            M.force value
    go argument

defaultedCaseM :: Bindings -> M.Th a -> M.MemoristM a
defaultedCaseM bindings argument = do
    M.tick
    left <- M.force (integerCache bindings)
    right <- M.force (integerCache bindings)
    if left + right > 0 then M.force argument else M.force argument

defaultedGuardM :: Bindings -> M.Th a -> M.MemoristM a
defaultedGuardM bindings argument = do
    M.tick
    left <- M.force (integerCache bindings)
    right <- M.force (integerCache bindings)
    if left + right > 0 then M.force argument else M.force argument

bottomCaseM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a Int)
bottomCaseM _ argument = do
    M.tick
    local <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument local)

bottomGuardM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a Int)
bottomGuardM _ argument = do
    M.tick
    local <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument local)

computedBottomCaseM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a Int)
computedBottomCaseM _ argument = do
    M.tick
    matched <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument matched)

computedBottomGuardM :: Bindings -> M.Th a -> M.MemoristM (M.PairT a Int)
computedBottomGuardM _ argument = do
    M.tick
    matched <- M.lazy $ M.lazy (pure (undefined :: Int)) >>= workValue
    pure (M.PairT argument matched)
