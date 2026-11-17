module Generated (oneM, twoM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

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

oneM :: M.Th Int -> M.MemoristM Int
oneM = oneWithBindings globalBindings

oneWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
oneWithBindings (Bindings shared) argument = do
    M.tick
    M.PairT number _ <- M.force shared
    n <- M.force number
    x <- M.force argument
    pure (n + x)

twoM :: M.Th Int -> M.MemoristM Int
twoM = twoWithBindings globalBindings

twoWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
twoWithBindings (Bindings shared) argument = do
    M.tick
    M.PairT number _ <- M.force shared
    n <- M.force number
    x <- M.force argument
    pure (n + x)
