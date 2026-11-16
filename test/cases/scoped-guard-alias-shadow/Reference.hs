{-# LANGUAGE ScopedTypeVariables #-}
module Generated (observedM) where

import qualified Memorist.Memorist as M

ignoreM :: M.Th t -> M.MemoristM Int
ignoreM _ = M.tick >> pure 1

observedM :: forall a. M.Th a -> M.MemoristM Int
observedM x = do
    M.tick
    let y = x
        inner :: forall a. M.Th a -> M.MemoristM Int
        inner _ = M.tick >> ignoreM y
    argument <- M.lazy (pure True)
    inner argument
