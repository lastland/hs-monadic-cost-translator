module Generated (Scalar, oneM, brokenTupleM, brokenAbstractM, brokenFixedM, paidM) where

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

oneM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
oneM _ _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            second <- M.lazy (pure (7 :: Int))
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenTupleM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
brokenTupleM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy (error "constant-field tuple failure")
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenAbstractM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
brokenAbstractM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (error "constant-field abstract failure")
            second <- M.lazy (pure (7 :: Int))
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenFixedM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
brokenFixedM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            second <- M.lazy (error "constant-field fixed failure")
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

paidM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
paidM _ _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            second <- M.lazy (M.lazy (pure (5 :: Int)) >>= expensiveM)
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)
