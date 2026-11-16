-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

-- Internal spelling is immaterial to the source's scalar entry point.
data Box a = Box (M.Th a)

readBoxM :: M.Th (Box a) -> M.MemoristM a
readBoxM box = do
    M.tick
    value <- M.force box
    case value of
        Box field -> M.force field

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    box <- M.lazy (pure (Box input))
    readBoxM box
