module Generated (Bindings, initialize, asIntM) where

import qualified Memorist.Memorist as M

-- The scenario uses only Int; every exported call receives this same thunk.
newtype Bindings = Bindings (M.Th Int)

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    argument <- M.lazy (pure 18446744073709551616)
    expensiveM argument)

expensiveM :: M.Th Int -> M.MemoristM Int
expensiveM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

asIntM :: Bindings -> M.Th Int -> M.MemoristM Int
asIntM (Bindings shared) argument = do
    M.tick
    n <- M.force shared
    x <- M.force argument
    pure (n + x)
