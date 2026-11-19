{-# LANGUAGE ImplicitParams #-}
module LocalImplicitParameter where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

draw :: M.Th Int -> Q.Gen ()
draw t =
    let ?unused = M.tval t
    in Q.elements [()]

law :: M.Th Int -> Q.Property
law t = let observed = M.tval t in Q.property (observed == 3)

ordinary :: M.Th Int -> Int
ordinary t = M.tval t
