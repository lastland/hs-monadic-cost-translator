module Generated (asListM, asMaybeM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

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

asListM :: M.Th Int -> M.MemoristM Int
asListM = asListWithBindings globalBindings

asListWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
asListWithBindings bindings argument = do
    M.tick
    M.PairT number _ <- M.force (listShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)

asMaybeM :: M.Th Int -> M.MemoristM Int
asMaybeM = asMaybeWithBindings globalBindings

asMaybeWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
asMaybeWithBindings bindings argument = do
    M.tick
    M.PairT number _ <- M.force (maybeShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)
