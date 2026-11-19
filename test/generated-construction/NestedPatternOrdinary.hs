{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module NestedPatternOrdinary where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

unusedNumber :: forall a. Num a => a -> Q.Property
unusedNumber _ =
    let (seen, _) =
            (True, 1 :: a)
    in Q.property seen

shared :: M.Th Bool -> Q.Property
shared t =
    let (seen, flag) = (M.tval t, False)
        shrink () = [seen]
    in Q.property seen

commonInput :: M.Th Bool -> Q.Property
commonInput t =
    let observed = M.tval t
        (seen, flag) = (observed, observed)
        shrink () = [flag]
    in Q.property seen

opaque :: M.Th Bool -> Q.Property
opaque t =
    let (seen, flag) = if M.tval t then (True, False) else (False, True)
        shrink () = [flag]
    in Q.property seen

deadField :: M.Th Bool -> Q.Property
deadField t =
    let (seen, unused) = (True, M.tval t)
    in Q.property seen

insideForce :: M.Th Bool -> Q.Property
insideForce t =
    let !_ = let unused () = M.tval t in True
    in Q.property True

ordinaryForce :: M.Th Bool -> Q.Property
ordinaryForce t =
    let shrink () = let !_ = M.tval t in [False]
    in Q.property True

sharedForce :: M.Th Bool -> Q.Property
sharedForce t =
    let !seen = M.tval t
        shrink () = [seen]
    in Q.property True

unforced :: M.Th Bool -> Q.Property
unforced t = let _ = M.tval t in Q.property True
