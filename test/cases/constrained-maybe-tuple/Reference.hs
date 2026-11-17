module Generated (Scalar, oneM, brokenTupleM, brokenLeftM, brokenRightM) where

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

oneM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a a)) (M.MaybeT (M.PairT a a)))
oneM _ _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            second <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenTupleM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a a)) (M.MaybeT (M.PairT a a)))
brokenTupleM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy (error "Maybe-tuple tuple failure")
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenLeftM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a a)) (M.MaybeT (M.PairT a a)))
brokenLeftM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (error "Maybe-tuple left failure")
            second <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenRightM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a a)) (M.MaybeT (M.PairT a a)))
brokenRightM _ = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            first <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            second <- M.lazy (error "Maybe-tuple right failure")
            pure (M.PairT first second)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)
