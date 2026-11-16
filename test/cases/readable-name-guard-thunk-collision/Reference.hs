-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    -- The variable pattern guard binds without inspecting its scrutinee.
    let value = input
    M.force value
