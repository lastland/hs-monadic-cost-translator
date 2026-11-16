module Generated (Bindings, initialize, firstM, secondM) where

import qualified Memorist.Memorist as M

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

firstM :: Bindings -> M.Th Int -> M.MemoristM Int
firstM (Bindings seed) argument = do
    M.tick
    n <- M.force seed
    x <- M.force argument
    pure (n + x)

secondM :: Bindings -> M.Th Int -> M.MemoristM Int
secondM (Bindings seed) argument = do
    M.tick
    n <- M.force seed
    x <- M.force argument
    pure (n + x)
