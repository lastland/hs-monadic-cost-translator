module OperandInstantiations where

import qualified Memorist.Memorist as M
import OperandEquations
import qualified Test.QuickCheck as Q

type Draw = Q.Gen Bool

runPair :: (Draw, Bool) -> M.MemoristM Bool
runPair = forcePair

runF :: Draw -> Bool
runF = strictF

runErased :: Bool -> (Q.Property, Bool) -> Bool
runErased = erased

prop_pair :: M.Th Bool -> Q.Property
prop_pair x = Q.property (M.value (M.eval1 (runPair (M.tval x `seq` Q.arbitrary, True))))

prop_relay :: M.Th Bool -> Q.Property
prop_relay x = Q.property (M.value (M.eval1
    (relay (M.tval x `seq` (Q.arbitrary :: Draw), True))))

prop_alias :: M.Th Bool -> Q.Property
prop_alias x = Q.property (M.value (M.eval1
    (pairAlias (M.tval x `seq` (Q.arbitrary :: Draw), True))))
