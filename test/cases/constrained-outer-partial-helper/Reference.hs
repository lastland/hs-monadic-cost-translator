{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, oneM, manyM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
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

sharedBody :: (Eq a, Num a) => M.MemoristM (M.MaybeT (L.ListT a))
sharedBody = do
    list <- M.lazy $ do
        headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        tailThunk <- M.lazy (pure L.NilT)
        pure (L.ConsT headThunk tailThunk)
    pure (M.JustT list)

oneM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
oneM input = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    callback <- M.lazy (pure (helper input))
    counts <- M.lazy $ do
        first <- M.lazy (pure 0)
        end <- M.lazy (pure L.NilT)
        pure (L.ConsT first end)
    ML.mapMM callback counts

manyM :: forall a. (Eq a, Num a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
manyM input counts = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    callback <- M.lazy (pure (helper input))
    ML.mapMM callback counts
