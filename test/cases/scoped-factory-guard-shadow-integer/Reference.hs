{-# LANGUAGE ScopedTypeVariables #-}
module Generated (observedM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

-- This fixture observes only the Integer dictionary specialization.
newtype Bindings = Bindings (M.Th Integer)

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    argument <- M.lazy (pure 18446744073709551616)
    workM argument)

workM :: M.Th Integer -> M.MemoristM Integer
workM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- workM zero
        pure (result + 1)

observedM :: M.Th Integer -> M.MemoristM Int
observedM = observedWithBindings globalBindings

observedWithBindings :: Bindings -> M.Th Integer -> M.MemoristM Int
observedWithBindings (Bindings shared) _ = do
    M.tick
    let inner :: forall a. M.Th a -> M.MemoristM Int
        inner _ = do
            M.tick
            y <- M.force shared
            pure (if y == 1 then 1 else 2)
    argument <- M.lazy (pure True)
    inner argument
