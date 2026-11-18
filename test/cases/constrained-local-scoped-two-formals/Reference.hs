module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- One views entry; one shared pair per invocation; one work entry per
-- demanded leaf. The two leaves are independent, and both copies alias them.
viewsM :: (Num a, Num b) => M.Th a -> M.Th b -> M.Th Int -> M.MemoristM (M.PairT (M.PairT a b) (M.PairT a b))
viewsM _ _ count = do
    M.tick
    shared <- M.lazy $ do
        left <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n < 0 then error "left failure" else 10)
        right <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then error "right failure" else 14)
        pure (M.PairT left right)
    pure (M.PairT shared shared)
