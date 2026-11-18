module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- Full demand costs 5: one views entry and two work entries per dictionary
-- partition. Selecting one leaf in each partition costs 3. Roots cost 1.
-- Copies alias within each partition, but the two partitions are independent.
viewsM :: Integral a => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.PairT a Int) (M.PairT a Int)) (M.PairT (M.PairT a Integer) (M.PairT a Integer)))
viewsM _ count = do
    M.tick
    intShared <- M.lazy $ do
        left <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n < 0 then 6 else 10)
        right <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 22 else 14)
        pure (M.PairT left right)
    integerShared <- M.lazy $ do
        left <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n < 0 then 6 else 10)
        right <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 22 else 14)
        pure (M.PairT left right)
    ints <- M.lazy (pure (M.PairT intShared intShared))
    integers <- M.lazy (pure (M.PairT integerShared integerShared))
    pure (M.PairT ints integers)
