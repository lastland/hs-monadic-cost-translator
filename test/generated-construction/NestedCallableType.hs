{-# LANGUAGE TypeFamilies #-}
module NestedCallableType where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

type family Observer a
type instance Observer Int = M.Th Int -> Int

draw :: Q.Gen (Observer Int)
draw = Q.elements [readField]
  where
    readField :: Observer Int
    readField = M.tval

law :: M.Th Int -> Q.Property
law t =
    let observe :: Observer Int
        observe x = M.tval x
    in Q.property (observe t == 3)
