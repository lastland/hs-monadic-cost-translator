-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as ML

-- The observable source equations instantiate Higher at lists of Int.
data Higher a = Higher (M.Th (ML.ListT a))

wrapM :: M.Th (ML.ListT a) -> M.MemoristM (Higher a)
wrapM contents = do
    M.tick
    pure (Higher contents)

firstM :: M.Th (Higher a) -> M.MemoristM a
firstM higher = do
    M.tick
    value <- M.force higher
    case value of
        Higher contents -> do
            list <- M.force contents
            case list of
                ML.ConsT first _ -> M.force first
                ML.NilT -> error "first: expected nonempty list"

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    list <- M.lazy $ do
        rest <- M.lazy (pure ML.NilT)
        pure (ML.ConsT input rest)
    higher <- M.lazy (wrapM list)
    firstM higher
