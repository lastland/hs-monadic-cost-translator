{-# LANGUAGE TypeFamilies #-}
-- Handwritten call-by-need equations: variable arguments/fields keep their
-- thunk; computations get a suspension. Only source function calls tick.
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data BoxT a = BoxT (M.Th a) (M.Th a)
data BoxD a = BoxD (M.ThD a) (M.ThD a)

instance M.Demand a => M.Demand (BoxT a) where
    type Dem (BoxT a) = BoxD (M.Dem a)
    fullDemand (BoxT x y) = BoxD (M.fullDemand x) (M.fullDemand y)
    usage (BoxT x y) (BoxD dx dy) = M.union (M.usage x dx) (M.usage y dy)

ignoreM :: M.Th a -> M.MemoristM Int
ignoreM _ = M.tick >> pure 42

plainM, parenthesizedM, annotatedM :: M.Th a -> M.MemoristM Int
plainM x = M.tick >> ignoreM x
parenthesizedM x = M.tick >> ignoreM x
annotatedM x = M.tick >> ignoreM x

badM :: M.Th a -> M.MemoristM a
badM _ = M.tick >> error "computed argument or field evaluated"

computedM :: M.Th a -> M.MemoristM Int
computedM x = do
    M.tick
    unused <- M.lazy (badM x)
    ignoreM unused

constructorM, recordM, mixedM :: M.Th a -> M.MemoristM (BoxT a)
constructorM x = M.tick >> pure (BoxT x x)
recordM x = M.tick >> pure (BoxT x x)
mixedM x = do
    M.tick
    unused <- M.lazy (badM x)
    pure (BoxT x unused)

tupleM :: M.Th a -> M.MemoristM (M.PairT a a)
tupleM x = M.tick >> pure (M.PairT x x)

listM :: M.Th a -> M.MemoristM (L.ListT a)
listM x = do
    M.tick
    rest <- M.lazy $ do
        nil <- M.lazy (pure L.NilT)
        pure (L.ConsT x nil)
    pure (L.ConsT x rest)

consM :: M.Th a -> M.Th (L.ListT a) -> M.MemoristM (L.ListT a)
consM x xs = M.tick >> pure (L.ConsT x xs)

pairM :: M.Th a -> M.Th b -> M.MemoristM (M.PairT a b)
pairM x y = M.tick >> pure (M.PairT x y)

keepM :: M.Th a -> M.MemoristM a
keepM x = M.tick >> M.force x

partialM, composedM :: M.Th a -> M.Th (L.ListT b)
    -> M.MemoristM (L.ListT (M.PairT a b))
partialM x xs = do
    M.tick
    function <- M.lazy (pure (pairM x))
    mapReference function xs
composedM x xs = do
    M.tick
    function <- M.lazy $ pure $ \y -> do
        kept <- M.lazy (keepM y)
        pairM x kept
    mapReference function xs

-- Library map charges once per inspected list constructor, including Nil.
-- Its adapter does not charge an extra source invocation.
mapReference :: M.Th (M.Th a -> M.MemoristM b) -> M.Th (L.ListT a)
    -> M.MemoristM (L.ListT b)
mapReference function xs = do
    M.tick
    value <- M.force xs
    case value of
        L.NilT -> pure L.NilT
        L.ConsT x rest -> do
            apply <- M.force function
            y <- M.lazy (apply x)
            ys <- M.lazy (mapReference function rest)
            pure (L.ConsT y ys)

localM :: M.Th a -> M.MemoristM (M.PairT a a)
localM x = do
    M.tick
    shared <- M.lazy (keepM x)
    pairM shared shared
