module Generated (Scalar, expensiveM, makeM, oneM, buildM, viewsM, brokenHeadM, brokenTailM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

class Scalar a
instance Scalar Int
instance Scalar Integer

expensiveM :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensiveM input = do
    M.tick
    n <- M.force input
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

makeM :: (Eq a, Num a) => M.Th Int -> M.MemoristM (M.MaybeT a)
makeM input = do
    M.tick
    n <- M.force input
    if n == 0 then pure M.NothingT else do
        payload <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        pure (M.JustT payload)

oneM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
oneM _ _ = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    pure (M.PairT shared shared)

buildM :: (Eq a, Num a) => M.Th Int -> M.MemoristM (L.ListT a)
buildM input = do
    M.tick
    n <- M.force input
    if n <= 0 then pure L.NilT else do
        headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        tailThunk <- M.lazy $ do
            next <- M.lazy (pure (n - 1))
            buildM next
        pure (L.ConsT headThunk tailThunk)

viewsM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
viewsM _ input = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy (buildM input)
        pure (M.JustT list)
    pure (M.PairT shared shared)

brokenHeadM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
brokenHeadM _ = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (error "Maybe-list head failure")
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    pure (M.PairT shared shared)

brokenTailM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
brokenTailM _ = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            tailThunk <- M.lazy (error "Maybe-list tail failure")
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    pure (M.PairT shared shared)
