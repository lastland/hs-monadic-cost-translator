module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as List
import qualified Memorist.Memorist as M

keepM :: M.Th a -> M.MemoristM Bool
keepM _ = M.tick >> pure True

filteredM :: M.Th a -> M.MemoristM a
filteredM x = do
    M.tick
    predicate <- M.lazy (pure keepM)
    xs <- M.lazy $ do
        tailThunk <- M.lazy (error "undefined list tail")
        pure (L.ConsT x tailThunk)
    ys <- M.lazy (List.filterM predicate xs)
    List.headM ys
