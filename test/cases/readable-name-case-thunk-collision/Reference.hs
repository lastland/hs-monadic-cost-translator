-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    -- A variable-only case pattern binds the original, still-lazy argument.
    let z = input
    caseThunk0M z
  where
    caseThunk0M argument = do
        M.tick
        value <- M.force argument
        pure (value + 1)
