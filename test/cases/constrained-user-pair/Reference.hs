{-# LANGUAGE TypeFamilies #-}
module Generated (PairT(..), PairD(..), Scalar, oneM, brokenAbstractM, brokenPairM, twinsM) where

import qualified Memorist.Memorist as M

data PairT a b = PairT (M.Th a) (M.Th b)

data PairD a b = PairD (M.ThD a) (M.ThD b)
    deriving (Eq, Show)

instance (M.Demand a, M.Demand b) => M.Demand (PairT a b) where
    type Dem (PairT a b) = PairD (M.Dem a) (M.Dem b)
    fullDemand (PairT left right) = PairD (M.fullDemand left) (M.fullDemand right)
    usage (PairT left right) (PairD wantLeft wantRight) =
        M.union (M.usage left wantLeft) (M.usage right wantRight)

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

oneM :: (Eq a, Num a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
oneM _ fixed = do
    M.tick
    shared <- M.lazy $ do
        pair <- M.lazy $ do
            left <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            pure (PairT left fixed)
        pure (M.JustT pair)
    pure (M.PairT shared shared)

brokenAbstractM :: (Eq a, Num a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
brokenAbstractM _ fixed = do
    M.tick
    shared <- M.lazy $ do
        pair <- M.lazy $ do
            left <- M.lazy (error "user-Pair abstract failure")
            pure (PairT left fixed)
        pure (M.JustT pair)
    pure (M.PairT shared shared)

brokenPairM :: (Eq a, Num a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
brokenPairM _ _ = do
    M.tick
    shared <- M.lazy $ do
        pair <- M.lazy (error "user-Pair root failure")
        pure (M.JustT pair)
    pure (M.PairT shared shared)

twinsM :: (Eq a, Num a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)))
twinsM _ = do
    M.tick
    shared <- M.lazy $ do
        pair <- M.lazy $ do
            left <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            right <- M.lazy (M.lazy (pure 5) >>= expensiveM)
            pure (PairT left right)
        pure (M.JustT pair)
    pure (M.PairT shared shared)
