module Generated (observedM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

-- The scalar facade observes the explicitly Num-constrained function at Int.
newtype Bindings = Bindings (M.Th Int)

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (M.lazy (pure 5) >>= costlyM)

costlyM :: M.Th Int -> M.MemoristM Int
costlyM argument = do
    M.tick
    n <- M.force argument
    pure (n + n)

ignoreM :: M.Th unused -> M.Th Int -> M.MemoristM Int
ignoreM _ argument = M.tick >> M.force argument

observedM :: M.Th Int -> M.MemoristM Int
observedM = observedWithBindings globalBindings

observedWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
observedWithBindings (Bindings shared) argument = do
    M.tick
    unused <- M.lazy (do
        first <- M.force shared
        second <- M.force shared
        pure (first + second))
    ignoreM unused argument
