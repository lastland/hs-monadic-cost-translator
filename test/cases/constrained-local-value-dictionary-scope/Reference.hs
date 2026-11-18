module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- Full demand costs 3: one views entry and one work entry per shared leaf.
-- Local values and dictionary applications add no ticks. Roots cost 1, and
-- fully demanding a paid captured count costs 4. Both copies alias outer.
viewsM :: Integral a => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT a Int) (M.PairT a Int))
viewsM _ count = do
    M.tick
    outer <- M.lazy $ do
        inner <- M.lazy $ do
            left <- M.lazy $ do
                M.tick
                n <- M.force count
                pure (if n < 0 then 6 else 10)
            right <- M.lazy $ do
                M.tick
                n <- M.force count
                pure (if n == 0 then 22 else 14)
            pure (M.PairT left right)
        M.force inner
    pure (M.PairT outer outer)
