{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module OperandUpdateAlternatives where

import qualified Test.QuickCheck as Q

data Mixed a = Soft { slot :: a } | Hard { slot :: !a }
data Other a = Other { slot :: a }

changeMixed :: Mixed Bool -> a -> Mixed a
changeMixed r x = r { slot = x }

changeOther :: Other Bool -> a -> Other a
changeOther r x = r { slot = x }

prop_mixed :: Mixed Bool -> Q.Property -> Q.Property
prop_mixed r claim = changeMixed r claim `seq` Q.property True

prop_other :: Other Bool -> Q.Property -> Q.Property
prop_other r claim = changeOther r claim `seq` Q.property True
