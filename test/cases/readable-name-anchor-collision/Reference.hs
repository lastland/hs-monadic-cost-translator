-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    x <- M.lazy $ do
        value <- M.force input
        pure (value + 1)
    M.force x
