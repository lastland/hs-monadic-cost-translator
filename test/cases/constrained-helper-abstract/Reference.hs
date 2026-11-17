{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Scalar, oneM, twiceM) where

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

oneM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT a a)
oneM input = do
    M.tick
    helperM input
  where
    helperM :: forall b. (Eq b, Num b) => M.Th b -> M.MemoristM (M.PairT b b)
    helperM _ = do
        M.tick
        shared <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        pure (M.PairT shared shared)

twiceM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.PairT a a) (M.PairT a a))
twiceM input = do
    M.tick
    first <- M.lazy (helperM input)
    second <- M.lazy (helperM input)
    pure (M.PairT first second)
  where
    helperM :: forall b. (Eq b, Num b) => M.Th b -> M.MemoristM (M.PairT b b)
    helperM _ = do
        M.tick
        shared <- M.lazy (M.lazy (pure 5) >>= expensiveM)
        pure (M.PairT shared shared)
