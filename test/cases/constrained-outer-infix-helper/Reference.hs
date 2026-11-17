{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, oneM, twiceM, cachedHelperM, separateM, brokenHeadM, brokenRootM, symbolicM, mixedM) where

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

sharedBody :: (Eq a, Num a) => M.MemoristM (M.MaybeT (L.ListT a))
sharedBody = do
    list <- M.lazy $ do
        headThunk <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        tailThunk <- M.lazy (pure L.NilT)
        pure (L.ConsT headThunk tailThunk)
    pure (M.JustT list)

oneM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
oneM input n = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    helper input n

twiceM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
twiceM input n = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input n)
    second <- M.lazy (helper input n)
    pure (M.PairT first second)

cachedHelperM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
cachedHelperM input n = do
    M.tick
    shared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    result <- M.lazy (helper input n)
    pure (M.PairT result result)

separateM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
separateM input n = do
    M.tick
    leftShared <- M.lazy sharedBody
    rightShared <- M.lazy sharedBody
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT leftShared rightShared)
    first <- M.lazy (helper input n)
    second <- M.lazy (helper input n)
    pure (M.PairT first second)

brokenHeadM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
brokenHeadM input n = do
    M.tick
    shared <- M.lazy $ do
        list <- M.lazy $ do
            headThunk <- M.lazy (error "infix-helper head failure")
            tailThunk <- M.lazy (pure L.NilT)
            pure (L.ConsT headThunk tailThunk)
        pure (M.JustT list)
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input n)
    second <- M.lazy (helper input n)
    pure (M.PairT first second)

brokenRootM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
brokenRootM input n = do
    M.tick
    shared <- M.lazy (error "infix-helper root failure")
    let helper :: M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
        helper _ _ = M.tick >> pure (M.PairT shared shared)
    first <- M.lazy (helper input n)
    second <- M.lazy (helper input n)
    pure (M.PairT first second)

symbolicM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
symbolicM = oneM

mixedM :: forall a. (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
mixedM = twiceM
