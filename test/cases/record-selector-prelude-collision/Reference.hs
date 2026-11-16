-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import Prelude hiding (mapM)
import qualified Memorist.Memorist as M

data R = R (M.Th Int)

mapM :: M.Th R -> M.MemoristM Int
mapM record = do
    M.tick
    value <- M.force record
    case value of
        R field -> M.force field

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    record <- M.lazy (pure (R input))
    mapM record
