module Generated (Scalar, viewsM, brokenM) where

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

viewsM :: (Eq a, Num a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT a) (M.MaybeT a))
viewsM _ input = do
    M.tick
    shared <- M.lazy (makeM input)
    pure (M.PairT shared shared)

brokenM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT a) (M.MaybeT a))
brokenM _ = do
    M.tick
    shared <- M.lazy $ do
        payload <- M.lazy (error "abstract Maybe payload failure")
        pure (M.JustT payload)
    pure (M.PairT shared shared)
