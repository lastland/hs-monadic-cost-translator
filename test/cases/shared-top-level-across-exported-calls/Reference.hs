module Generated (firstM, secondM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

newtype Bindings = Bindings (M.Th Int)

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    five <- M.lazy (pure 5)
    doubleM five)

doubleM :: M.Th Int -> M.MemoristM Int
doubleM argument = do
    M.tick
    x <- M.force argument
    pure (x + x)

firstM :: M.Th Int -> M.MemoristM Int
firstM = firstWithBindings globalBindings

firstWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
firstWithBindings (Bindings seed) argument = do
    M.tick
    n <- M.force seed
    x <- M.force argument
    pure (n + x)

secondM :: M.Th Int -> M.MemoristM Int
secondM = secondWithBindings globalBindings

secondWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
secondWithBindings (Bindings seed) argument = do
    M.tick
    n <- M.force seed
    x <- M.force argument
    pure (n + x)
