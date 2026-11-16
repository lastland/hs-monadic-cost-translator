module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

chooseM :: M.Th (L.ListT a) -> M.MemoristM a
chooseM xs = do
    M.tick
    if False then error "first equation" else do
        list <- M.force xs
        case list of
            L.NilT -> error "empty-list equation"
            L.ConsT y _
                | True, False -> error "conjunctive guard"
                | True -> M.force y

orderedM :: M.Th a -> M.MemoristM a
orderedM x = do
    M.tick
    xs <- M.lazy $ do
        rest <- M.lazy (error "undefined list tail")
        pure (L.ConsT x rest)
    chooseM xs
