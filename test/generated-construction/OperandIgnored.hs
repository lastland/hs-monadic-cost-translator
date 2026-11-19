module OperandIgnored where

import qualified Memorist.Memorist as M
import OperandEquations (ignoreField)
import qualified Test.QuickCheck as Q

keepFlag :: (Q.Property, Bool) -> Bool
keepFlag = ignoreField

prop_ignored :: M.Th Bool -> Q.Property
prop_ignored x =
    let flag = M.tval x
        payload = Q.property flag
    in Q.property (ignoreField (payload, True))
