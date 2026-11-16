-- Independent call-by-need model of Example.hs. Every source invocation owns
-- its tick; the helpers implementing constructor construction and composition
-- introduce no fictitious source calls.
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

workM :: M.Th Int -> M.MemoristM Int
workM x = do
    M.tick
    n <- M.force x
    pure (n + n)

keepM :: M.Th a -> M.MemoristM a
keepM x = M.tick >> M.force x

type IntPair = M.PairT Int Int

returnedSharedM :: M.Th Int -> M.MemoristM IntPair
returnedSharedM x = do
    M.tick
    y <- M.lazy (workM x)
    a <- M.lazy (keepM y)
    b <- M.lazy (keepM y)
    pure (M.PairT a b)

aliasM, caseAliasM, guardAliasM :: M.Th a -> M.MemoristM (M.PairT a a)
aliasM x = M.tick >> pure (M.PairT x x)
caseAliasM x = M.tick >> pure (M.PairT x x)
guardAliasM x = M.tick >> pure (M.PairT x x)

separateM, localPairM, caseLocalM, caseAnnotatedM, guardLocalM,
    guardAnnotatedM, computedCaseM, computedGuardM ::
    M.Th Int -> M.MemoristM IntPair
separateM x = do
    M.tick
    a <- M.lazy (workM x)
    b <- M.lazy (workM x)
    pure (M.PairT a b)
localPairM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
caseLocalM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
caseAnnotatedM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
guardLocalM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
guardAnnotatedM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
computedCaseM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
computedGuardM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)

pairM :: M.Th a -> M.Th b -> M.MemoristM (M.PairT a b)
pairM a b = M.tick >> pure (M.PairT a b)

-- The source map invokes one equation for each cons and for nil. Calls to f
-- and recursive calls remain suspended until their output fields are needed.
mapSource :: (M.Th a -> M.MemoristM b) -> M.Th (L.ListT a) ->
    M.MemoristM (L.ListT b)
mapSource f input = do
    M.tick
    spine <- M.force input
    case spine of
        L.NilT -> pure L.NilT
        L.ConsT h t -> do
            headResult <- M.lazy (f h)
            tailResult <- M.lazy (mapSource f t)
            pure (L.ConsT headResult tailResult)

capturedM, computedCaptureM, specializedM, nestedSpecializedM ::
    M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT IntPair)
capturedM x xs = do
    M.tick
    mapSource (pairM x) xs
computedCaptureM x xs = do
    M.tick
    capture <- M.lazy (workM x)
    mapSource (pairM capture) xs
specializedM x xs = do
    M.tick
    -- (pair . work) x captures one lazy work application, not one per item.
    capture <- M.lazy (workM x)
    mapSource (pairM capture) xs
nestedSpecializedM x xs = do
    M.tick
    inner <- M.lazy (workM x)
    capture <- M.lazy (workM inner)
    mapSource (pairM capture) xs
