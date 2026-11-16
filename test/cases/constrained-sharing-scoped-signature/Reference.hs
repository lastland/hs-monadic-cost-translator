module Generated (Bindings, initialize, observedM) where

import qualified Memorist.Memorist as M

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

observedM :: Bindings -> M.Th Int -> M.MemoristM Int
observedM (Bindings shared) argument = do
    M.tick
    unused <- M.lazy (do
        first <- M.force shared
        second <- M.force shared
        pure (first + second))
    ignoreM unused argument
