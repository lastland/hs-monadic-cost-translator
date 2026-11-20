module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- Each go invocation suspends its own paid count field. Both result views
-- select that field, so demanding either pays once; the key payload stays lazy.
viewsM :: Integral a => M.Th a -> M.Th t -> M.Th Int -> M.MemoristM (M.PairT Int Int)
viewsM _ _ count = do
    M.tick
    n <- M.force count
    if n == 0 then go True else go 'c'
  where
    go _ = do
        M.tick
        field <- M.lazy (M.tick >> M.force count)
        M.tick
        pure (M.PairT field field)
