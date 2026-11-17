module Generated (Scalar, oneM, brokenInnerM, brokenLeafM) where

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

oneM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
oneM _ input = do
    M.tick
    shared <- M.lazy $ do
        inner <- M.lazy (makeM input)
        pure (M.JustT inner)
    pure (M.PairT shared shared)

brokenInnerM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
brokenInnerM _ = do
    M.tick
    shared <- M.lazy $ do
        inner <- M.lazy (error "nested Maybe inner failure")
        pure (M.JustT inner)
    pure (M.PairT shared shared)

brokenLeafM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
brokenLeafM _ = do
    M.tick
    shared <- M.lazy $ do
        inner <- M.lazy $ do
            leaf <- M.lazy (error "nested Maybe leaf failure")
            pure (M.JustT leaf)
        pure (M.JustT inner)
    pure (M.PairT shared shared)
