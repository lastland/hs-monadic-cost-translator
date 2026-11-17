{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, oneM, twiceM, cachedHelperM, manyM, separateM, brokenHeadM, brokenRootM) where

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

oneM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
oneM input _ = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
    helper input

twiceM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
twiceM input = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input)
    second <- M.lazy (helper input)
    pure (M.PairT first second)

sharedBody :: (Eq a, Num a) => M.MemoristM (M.MaybeT (L.ListT a))
sharedBody = do
    list <- M.lazy $ do
        headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        tailThunk <- M.lazy (pure L.NilT)
        pure (L.ConsT headThunk tailThunk)
    pure (M.JustT list)

cachedHelperM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
cachedHelperM input = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
    result <- M.lazy (helper input)
    pure (M.PairT result result)

manyM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
manyM input count = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
        loop :: M.Th Int -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
        loop argument = do
            M.tick
            n <- M.force argument
            if n <= 0 then pure L.NilT else do
                headThunk <- M.lazy (helper input)
                tailThunk <- M.lazy $ do
                    next <- M.lazy (pure (n - 1))
                    loop next
                pure (L.ConsT headThunk tailThunk)
    loop count

separateM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
separateM input = do
    M.tick
    leftShared <- M.lazy sharedBody
    rightShared <- M.lazy sharedBody
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT leftShared rightShared)
    first <- M.lazy (helper input)
    second <- M.lazy (helper input)
    pure (M.PairT first second)

brokenHeadM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
brokenHeadM input = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (error "outer-helper head failure")
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input)
    second <- M.lazy (helper input)
    pure (M.PairT first second)

brokenRootM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
brokenRootM input = do
    M.tick
    shared <- M.lazy (error "outer-helper root failure")
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input)
    second <- M.lazy (helper input)
    pure (M.PairT first second)
