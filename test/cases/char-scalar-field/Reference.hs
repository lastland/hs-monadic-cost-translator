-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Box = Box (M.Th Char)

readBoxM :: M.Th Box -> M.MemoristM Int
readBoxM box = do
    M.tick
    value <- M.force box
    case value of
        Box _ -> pure 5

observedM :: M.Th Int -> M.MemoristM Int
observedM _ = do
    M.tick
    box <- M.lazy $ do
        character <- M.lazy (pure 'a')
        pure (Box character)
    readBoxM box
