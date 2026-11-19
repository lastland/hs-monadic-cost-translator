module Generated (viewsM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Each producer reads the shared count and pays one tick at every recursive
-- entry, including zero. Its Foldable dictionary is unused, not interchangeable.
makeM :: M.Th Int -> M.MemoristM (M.MaybeT a)
makeM count = do
    M.tick
    n <- M.force count
    if n < 0 then error "negative count"
    else if n == 0 then pure M.NothingT
    else M.lazy (pure (n - 1)) >>= makeM

viewsM :: M.Th Int -> M.MemoristM
    (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
        (M.MaybeT (M.MaybeT Int)))
viewsM count = do
    M.tick
    listRoot <- M.lazy (makeM count)
    maybeRoot <- M.lazy (makeM count)
    aliases <- M.lazy (pure (M.PairT listRoot listRoot))
    pure (M.PairT aliases maybeRoot)
