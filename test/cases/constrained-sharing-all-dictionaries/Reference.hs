module Generated (oneM, twoM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

data Bindings = Bindings
    { intIntegerShared :: M.Th (M.PairT Int Integer)
    , intIntShared :: M.Th (M.PairT Int Int)
    }

initialize :: M.MemoristM Bindings
initialize = do
    intInteger <- M.lazy sharedM
    intInt <- M.lazy sharedM
    pure (Bindings intInteger intInt)

-- Both constrained variables participate in sharing, even when the second
-- field is not demanded by either scalar facade.
sharedM :: (Num a, Num b) => M.MemoristM (M.PairT a b)
sharedM = do
    first <- M.lazy (M.lazy (pure 5) >>= workM)
    second <- M.lazy (M.lazy (pure 5) >>= workM)
    pure (M.PairT first second)

workM :: Num a => M.Th a -> M.MemoristM a
workM argument = do
    M.tick
    n <- M.force argument
    pure (n + n)

oneM :: M.Th Int -> M.MemoristM Int
oneM = oneWithBindings globalBindings

oneWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
oneWithBindings bindings argument = do
    M.tick
    M.PairT number _ <- M.force (intIntegerShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)

twoM :: M.Th Int -> M.MemoristM Int
twoM = twoWithBindings globalBindings

twoWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
twoWithBindings bindings argument = do
    M.tick
    M.PairT number _ <- M.force (intIntShared bindings)
    n <- M.force number
    x <- M.force argument
    pure (n + x)
