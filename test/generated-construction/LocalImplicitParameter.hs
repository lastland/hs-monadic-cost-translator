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

called :: M.Th Int -> Q.Property
called t = let ?observe = (M.tval :: M.Th Int -> Int)
           in Q.property (?observe t == 3)

wrongCarrier :: Q.Gen ()
wrongCarrier = let ?discard = pure () :: [()] in Q.elements [()]

shared :: M.Th Int -> Q.Property
shared t = let observed = M.tval t
           in let ?inspect = observed in Q.property (observed == 3)

constant :: Q.Property
constant = let ?number = (3 :: Int) in Q.property (?number == 3)
