{-# LANGUAGE BangPatterns #-}
module NestedPatternConsumption where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

forced :: M.Th Bool -> Q.Property
forced t = let !_ = M.tval t in Q.property True

split :: M.Th Bool -> Q.Property
split t =
    let (seen, flag) = (M.tval t, False)
        shrink () = [flag]
    in Q.property seen

nested :: M.Th Bool -> Q.Property
nested t =
    let observed = M.tval t
        ((flag, seen), _) = ((False, observed), ())
        shrink () = [flag]
    in Q.property seen

selected :: M.Th Bool -> Q.Property
selected t =
    let (seen, flag) = (t == t, False)
        shrink () = [flag]
    in Q.property seen

forcedAlias :: M.Th Bool -> Q.Property
forcedAlias t =
    let observed = M.tval t
        !seen = observed
    in Q.property True

withFunction :: M.Th Bool -> Q.Property
withFunction t =
    let (seen, f) = (M.tval t, id :: Bool -> Bool)
    in Q.property seen

withAction :: M.Th Bool -> Q.Property
withAction t =
    let (seen, act) = (M.tval t, pure True :: M.MemoristM Bool)
    in Q.property seen

forcedUnit :: M.Th () -> Q.Property
forcedUnit t = let !() = M.tval t in Q.property True
