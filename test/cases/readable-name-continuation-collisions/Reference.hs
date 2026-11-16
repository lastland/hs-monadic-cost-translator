-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

chooseM :: M.Th Int -> M.MemoristM Int
chooseM input = do
    M.tick
    value <- M.force input
    case value of
        0 -> pure 0
        _ -> M.force input

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    value <- M.force input
    if value > 0 then chooseM input else pure 0
