module Generated (Scalar, oneM, brokenAbstractM) where

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

oneM :: (Eq a, Num a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)))
oneM _ fixed = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            left <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            pure (M.PairT left fixed)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)

brokenAbstractM :: (Eq a, Num a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)))
brokenAbstractM _ fixed = do
    M.tick
    shared <- M.lazy $ do
        tuple <- M.lazy $ do
            left <- M.lazy (error "captured-field abstract failure")
            pure (M.PairT left fixed)
        pure (M.JustT tuple)
    pure (M.PairT shared shared)
