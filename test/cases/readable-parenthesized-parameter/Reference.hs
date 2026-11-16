-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

observedM :: M.Th Int -> M.MemoristM Int
observedM source = do
    M.tick
    M.force source
