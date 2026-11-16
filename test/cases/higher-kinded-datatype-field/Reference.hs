-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as ML

-- Only the source's list instantiation reaches the scalar entry point.
data Higher a = Higher (M.Th a) (M.Th (ML.ListT a))

wrapM :: M.Th a -> M.Th (ML.ListT a) -> M.MemoristM (Higher a)
wrapM first rest = do
    M.tick
    pure (Higher first rest)

firstM :: M.Th (Higher a) -> M.MemoristM a
firstM higher = do
    M.tick
    value <- M.force higher
    case value of
        Higher _ rest -> do
            list <- M.force rest
            case list of
                ML.ConsT first _ -> M.force first
                ML.NilT -> error "first: expected nonempty list"

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    ignored <- M.lazy (pure undefined)
    list <- M.lazy $ do
        rest <- M.lazy (pure ML.NilT)
        pure (ML.ConsT input rest)
    higher <- M.lazy (wrapM ignored list)
    firstM higher
