module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- Full demand costs 3: one views entry and one work entry per shared leaf.
-- One selected leaf costs 2; roots only cost 1. Count is not needed for roots.
-- Each invocation allocates one pair; both returned copies alias its leaves.
viewsM :: Integral a => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT a Int) (M.PairT a Int))
viewsM _ count = do
    M.tick
    shared <- M.lazy $ do
        left <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n < 0 then 6 else 10)
        right <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 22 else 14)
        pure (M.PairT left right)
    pure (M.PairT shared shared)
