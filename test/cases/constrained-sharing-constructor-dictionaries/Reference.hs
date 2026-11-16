module Generated (Bindings, initialize, asListM, asMaybeM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data Bindings = Bindings
    { listShared :: M.Th (M.PairT Int (L.ListT Int))
    , maybeShared :: M.Th (M.PairT Int (M.MaybeT Int))
    }

initialize :: M.MemoristM Bindings
initialize = do
    list <- M.lazy sharedM
    maybe <- M.lazy sharedM
    pure (Bindings list maybe)

-- Distinct Foldable dictionaries allocate distinct pairs and work thunks.
-- Their undefined second fields must remain suspended.
sharedM :: M.MemoristM (M.PairT Int b)
sharedM = do
    number <- M.lazy (M.lazy (pure 5) >>= workM)
    ignored <- M.lazy (pure undefined)
    pure (M.PairT number ignored)

workM :: M.Th Int -> M.MemoristM Int
workM argument = do
    M.tick
    n <- M.force argument
    pure (n + 1)

asListM :: Bindings -> M.Th Int -> M.MemoristM Int
asListM bindings argument = do
    M.tick
    M.PairT number _ <- M.force (listShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)

asMaybeM :: Bindings -> M.Th Int -> M.MemoristM Int
asMaybeM bindings argument = do
    M.tick
    M.PairT number _ <- M.force (maybeShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)
