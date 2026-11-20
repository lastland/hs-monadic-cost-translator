module OperandPropertyLaws where

import qualified Test.QuickCheck as Q

strict :: a -> Bool
strict x = seq x True

law :: a -> Q.Property
law x = Q.property (strict x)

relay :: a -> Q.Property
relay = law

own :: a -> Q.Property
own x = seq x (Q.property True)
