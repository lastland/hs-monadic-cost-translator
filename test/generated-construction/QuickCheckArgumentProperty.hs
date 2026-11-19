module QuickCheckArgumentProperty where

import qualified Memorist.Memorist as M
import QuickCheckArgumentConsumer (apply)
import qualified Test.QuickCheck as Q

gen :: M.Th Bool -> Q.Gen Bool
gen x = M.tval x `seq` Q.arbitrary

prop_transfer :: M.Th Bool -> Q.Property
prop_transfer x = Q.property (M.value (M.eval1 (apply gen x)))
