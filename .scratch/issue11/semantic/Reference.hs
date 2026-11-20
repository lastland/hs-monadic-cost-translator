-- Independent account: entering twice costs one tick; evaluating its one
-- argument costs the producer tick once, even though addition consumes it twice.
module Reference (twiceM) where

import qualified Memorist.Memorist as M

twiceM :: Num a => M.Th a -> M.MemoristM a
twiceM argument = do
    M.tick
    value <- M.force argument
    pure (value + value)
