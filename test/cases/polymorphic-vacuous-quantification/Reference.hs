module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- The quantified variable does not occur in the value's type. There is one
-- ordinary Int thunk: views costs one; demanding either field runs work once.
-- Full/first/second cost 2, skipped fields cost 1, cached demands cost 2, and
-- separate invocations cost 2 each. A paid count adds one only when demanded.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT Int Int)
viewsM _ count = do
    M.tick
    shared <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (n + n)
    pure (M.PairT shared shared)
