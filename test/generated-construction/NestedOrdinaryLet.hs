module NestedOrdinaryLet where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

claim :: M.Th Int -> Q.Property
claim t =
    let draw = Q.elements [M.tval t]
        check n = Q.property (n == 3)
        unused :: Int -> [Int]
        unused n =
            pure n
        observe :: M.Th Int -> Int
        observe = M.tval
        dead = M.tval t
    in Q.forAll draw (\n -> check (n + observe t))

shared :: M.Th Int -> Q.Property
shared t =
    let observed = M.tval t
        shrink _ = [observed]
    in Q.property (observed == 3)
