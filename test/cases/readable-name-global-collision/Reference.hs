-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

doubleM :: M.Th Int -> M.MemoristM Int
doubleM input = do
    M.tick
    left <- M.force input
    right <- M.force input
    pure (left + right)

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    incremented <- M.lazy (localDoubleM input)
    doubleM incremented
  where
    -- The source's local doubleM is distinct from its global double.
    localDoubleM argument = do
        M.tick
        value <- M.force argument
        pure (value + 1)
