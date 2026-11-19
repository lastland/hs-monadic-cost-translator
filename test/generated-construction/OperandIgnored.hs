module OperandIgnored where

import qualified Memorist.Memorist as M
import OperandEquations (grow, ignoreField, strictF)
import qualified Test.QuickCheck as Q

keepFlag :: (Q.Property, Bool) -> Bool
keepFlag = ignoreField

prop_ignored :: M.Th Bool -> Q.Property
prop_ignored x =
    let flag = M.tval x
        payload = Q.property flag
    in Q.property (ignoreField (payload, True))

outerOnly :: Either (Q.Gen Bool) Bool -> Bool
outerOnly = strictF

ignoreThroughRecursion :: (Q.Property, Bool) -> Bool
ignoreThroughRecursion = grow True
