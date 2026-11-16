-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Box = Box (M.Th Int) (M.Th Int)

readBoxM :: M.Th Box -> M.MemoristM Int
readBoxM box = do
    M.tick
    value <- M.force box
    case value of
        Box payload _ -> M.force payload

payloadM :: M.Th Box -> M.MemoristM Int
payloadM box = do
    M.tick
    value <- M.force box
    case value of
        Box payload _ -> M.force payload

recordedM :: M.Th Int -> M.MemoristM Int
recordedM input = do
    M.tick
    -- Both readers share b and its payload. The other field stays bottom.
    box <- M.lazy $ do
        ignored <- M.lazy (pure undefined)
        pure (Box input ignored)
    left <- readBoxM box
    right <- payloadM box
    pure (left + right)
