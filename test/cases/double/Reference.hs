module Generated where

import qualified Memorist.Memorist as M

doubleM :: Num a => M.Th a -> M.MemoristM a
doubleM x = do
    M.tick
    a <- M.force x
    b <- M.force x
    pure (a + b)
