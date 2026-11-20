module OperandFieldSyntax where

import qualified Test.QuickCheck as Q

data Cell a b = Cell { hard :: !a, soft :: b }

labelled :: Q.Property -> Cell Bool Q.Property
labelled p = Cell { soft = p, hard = False }

boxed :: Q.Gen Bool -> (Bool, [Q.Gen Bool])
boxed draw = (False, [draw])

paired :: Q.Property -> (Q.Property, Bool)
paired p = (p, False)

strict :: Q.Property -> Cell Q.Property Bool
strict p = Cell { soft = False, hard = p }

forced :: Q.Property -> Cell Bool Q.Property
forced p = Cell { hard = False, soft = p `seq` p }
