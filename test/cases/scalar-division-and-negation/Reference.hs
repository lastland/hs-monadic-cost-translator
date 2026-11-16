module Generated where

import qualified Memorist.Memorist as M

arithmeticM :: M.Th Int -> M.MemoristM Int
arithmeticM x = do
    M.tick
    value <- M.force x
    pure (((value * 3) `div` 2) + negate 1)
