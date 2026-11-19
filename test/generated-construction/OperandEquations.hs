{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module OperandEquations where

import qualified Memorist.Memorist as M

forcePair :: (a, Bool) -> M.MemoristM Bool
forcePair (x, _) = x `seq` pure True

strictF :: f a -> Bool
strictF x = x `seq` True

erased :: Bool -> (a, Bool) -> Bool
erased _ _ = True
erased False (x, _) = x `seq` False

ignoreField :: (a, Bool) -> Bool
ignoreField (_unused, flag) = flag
