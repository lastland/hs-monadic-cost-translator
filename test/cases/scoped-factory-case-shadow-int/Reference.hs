{-# LANGUAGE ScopedTypeVariables #-}
module Generated (Bindings, initialize, observedM) where

import qualified Memorist.Memorist as M

-- This fixture observes only the Int dictionary specialization.
newtype Bindings = Bindings (M.Th Int)

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    argument <- M.lazy (pure 18446744073709551616)
    workM argument)

workM :: M.Th Int -> M.MemoristM Int
workM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- workM zero
        pure (result + 1)

observedM :: Bindings -> M.Th Int -> M.MemoristM Int
observedM (Bindings shared) _ = do
    M.tick
    let inner :: forall a. M.Th a -> M.MemoristM Int
        inner _ = do
            M.tick
            y <- M.force shared
            pure (if y == 1 then 1 else 2)
    argument <- M.lazy (pure True)
    inner argument
