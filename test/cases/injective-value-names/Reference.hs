-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

percentM :: M.Th Int -> M.Th Int -> M.MemoristM Int
percentM first second = do
    M.tick
    left <- M.force first
    right <- M.force second
    pure (left + right)

-- This source identifier denotes a different function from (%%).
operator_37_37M :: M.Th Int -> M.MemoristM Int
operator_37_37M input = do
    M.tick
    value <- M.force input
    pure (value + 1)

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    left <- percentM input input
    right <- operator_37_37M input
    pure (left + right)
