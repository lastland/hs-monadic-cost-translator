module QuickCheckUnusedArgument where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

ignore :: Q.Gen Bool -> (Bool -> Q.Property) -> M.MemoristM Bool
ignore _draw _makeProperty = pure True

gen :: M.Th Bool -> Q.Gen Bool
gen x = M.tval x `seq` Q.arbitrary

prop_unused :: M.Th Bool -> Q.Property
prop_unused x = Q.property (M.value (M.eval1 (ignore (gen x) Q.property)))
