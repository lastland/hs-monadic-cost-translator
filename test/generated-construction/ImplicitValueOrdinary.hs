{-# LANGUAGE ImplicitParams #-}
module ImplicitValueOrdinary where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

overridden :: M.Th Bool -> Q.Property
overridden t = let ?seen = M.tval t in
    let ?seen = True in Q.property ?seen

readSeen :: (?seen :: Bool) => () -> Bool
readSeen () = ?seen

shared :: M.Th Bool -> Q.Property
shared t = let ?seen = M.tval t in Q.property (?seen && readSeen ())

captured :: M.Th Bool -> Q.Property
captured t = let ?seen = M.tval t in
    let check :: () -> Bool
        check () = ?seen
    in Q.property (?seen && check ())

pass :: Bool -> Bool
pass x = x

handed :: M.Th Bool -> Q.Property
handed t = let ?seen = M.tval t in Q.property (pass ?seen)
