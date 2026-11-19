{-# LANGUAGE FlexibleInstances #-}
module NestedInstanceRoles where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

class Make a where
    make :: M.Th Int -> a

instance Make (Q.Gen Int) where
    make t = Q.elements [M.tval t]
      where
        deadShrink x = [M.tval x]

instance Make Q.Property where
    make t = let observed = M.tval t in Q.property (observed == 3)

instance Make Bool where
    make t = M.tval t == 3
