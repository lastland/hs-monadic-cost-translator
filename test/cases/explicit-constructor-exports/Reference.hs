-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Box = Box (M.Th Int)

makeM :: M.Th Int -> M.MemoristM Box
makeM input = do
    M.tick
    pure (Box input)

readBoxM :: M.Th Box -> M.MemoristM Int
readBoxM box = do
    M.tick
    value <- M.force box
    case value of
        Box field -> M.force field

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    box <- M.lazy (makeM input)
    readBoxM box
