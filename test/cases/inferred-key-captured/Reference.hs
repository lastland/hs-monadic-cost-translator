module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- The source's branch forces count before entering go. Both views select the
-- same original count thunk; neither the dictionary payload nor extra is used.
viewsM :: Integral a => M.Th a -> M.Th t -> M.Th Int -> M.MemoristM (M.PairT Int Int)
viewsM _ _ count = do
    M.tick
    n <- M.force count
    if n == 0 then go True else go 'c'
  where
    go _ = do
        M.tick
        M.tick
        pure (M.PairT count count)
