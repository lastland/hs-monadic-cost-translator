-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

-- The source's (%%) is an ordinary function, unlike primitive addition.
percentM :: M.Th Int -> M.Th Int -> M.MemoristM Int
percentM first second = do
    M.tick
    left <- M.force first
    right <- M.force second
    pure (left + right)

operatedM :: M.Th Int -> M.MemoristM Int
operatedM input = do
    M.tick
    percentM input input
