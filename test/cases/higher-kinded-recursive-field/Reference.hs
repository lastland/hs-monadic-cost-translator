-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as ML

-- Only Fix [] is used by observed; its recursive field remains lazy.
data Fix = In (M.Th (ML.ListT Fix))

rollM :: M.Th (ML.ListT Fix) -> M.MemoristM Fix
rollM contents = do
    M.tick
    pure (In contents)

isEmptyM :: M.Th Fix -> M.MemoristM Bool
isEmptyM fix = do
    M.tick
    value <- M.force fix
    case value of
        In contents -> do
            list <- M.force contents
            case list of
                ML.NilT -> pure True
                ML.ConsT _ _ -> pure False

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    empty <- M.lazy (pure ML.NilT)
    rolled <- M.lazy (rollM empty)
    condition <- isEmptyM rolled
    if condition then M.force input else pure 0
