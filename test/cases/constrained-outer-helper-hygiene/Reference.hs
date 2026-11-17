{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, PairT(..), oneM) where

import qualified Memorist.Memorist as M

data PairT a b = PairT (M.Th a) (M.Th b)

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

oneM :: forall a. (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)))
oneM input = do
    M.tick
    let helper :: M.Th a -> M.MemoristM (M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)))
        helper fixed = do
            M.tick
            shared <- M.lazy $ do
                pair <- M.lazy $ do
                    left <- M.lazy (M.lazy (pure 5) >>= expensiveM)
                    pure (PairT left fixed)
                pure (M.JustT pair)
            pure (M.PairT shared shared)
    helper input
