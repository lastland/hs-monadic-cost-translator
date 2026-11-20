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

independentFunction :: M.Th Bool -> Q.Property
independentFunction t =
    let observed = M.tval t
        (check, seen) =
            ( \other -> M.tval other &&
                        other == t
            , observed )
    in Q.property (seen && check t)

captured :: M.Th Bool -> Q.Property
captured t =
    let (seen, check) = (M.tval t, \() -> seen)
    in Q.property (seen && check ())

opaqueFunction :: M.Th Bool -> Q.Property
opaqueFunction t =
    let (seen, f) = if M.tval t then (True, id :: Bool -> Bool) else (False, not)
    in Q.property (f seen)

ordinary :: M.Th Bool -> Bool
ordinary t =
    let (seen, f) = (M.tval t, id :: Bool -> Bool)
    in f seen

independentAction :: M.Th Bool -> Q.Property
independentAction t =
    let (seen, act) =
            ( M.tval t
            , M.tval t `seq` (pure True :: M.MemoristM Bool) )
    in Q.property (seen && M.value (M.eval1 act))

annotatedNumber :: forall a. Num a => a -> Q.Property
annotatedNumber _ =
    let (seen :: Bool, _) =
            (True, 1 :: a)
    in Q.property seen

unitWithHelper :: Eq a => M.Th () -> a -> Q.Property
unitWithHelper t inherited =
    let observed = M.tval t
        !() = let check actual = actual == inherited in observed
    in Q.property True

sharedUnit :: M.Th () -> Q.Property
sharedUnit t =
    let observed = M.tval t
        !() = observed
        shrink () = [observed]
    in Q.property True
