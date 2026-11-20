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

replaceSoft :: Cell Bool a -> Q.Property -> Cell Bool Q.Property
replaceSoft cell p = cell { soft = p }

data Choice a = Loose { member :: a } | Tight { member :: !a }

replaceMixed :: Choice a -> Q.Property -> Choice Q.Property
replaceMixed choice p = choice { member = p }
