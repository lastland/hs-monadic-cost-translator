module Generated (Bindings, initialize, oneM, twoM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- The exported scalar observations never inspect the empty second field.
-- Erasing its element type is safe and keeps the Int-keyed tuple shared.
newtype Bindings = Bindings (M.Th (M.PairT Int (L.ListT ())))

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    number <- M.lazy (M.lazy (pure 18446744073709551616) >>= expensiveM)
    empty <- M.lazy (pure L.NilT)
    pure (M.PairT number empty))

expensiveM :: M.Th Int -> M.MemoristM Int
expensiveM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

oneM :: Bindings -> M.Th Int -> M.MemoristM Int
oneM (Bindings shared) argument = do
    M.tick
    M.PairT number _ <- M.force shared
    n <- M.force number
    x <- M.force argument
    pure (n + x)

twoM :: Bindings -> M.Th Int -> M.MemoristM Int
twoM (Bindings shared) argument = do
    M.tick
    M.PairT number _ <- M.force shared
    n <- M.force number
    x <- M.force argument
    pure (n + x)
