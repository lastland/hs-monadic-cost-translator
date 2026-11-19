module QuickCheckUnusedArgument where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

ignore :: Q.Gen Bool -> (Bool -> Q.Property) -> M.MemoristM Bool
ignore _draw _makeProperty = pure True

gen :: M.Th Bool -> Q.Gen Bool
gen x = M.tval x `seq` Q.arbitrary

prop_unused :: M.Th Bool -> Q.Property
prop_unused x = Q.property (M.value (M.eval1 (ignore (gen x) Q.property)))

ignoreAny :: a -> M.MemoristM Bool
ignoreAny _unused = pure True

ignoreDraw :: Q.Gen Bool -> M.MemoristM Bool
ignoreDraw = ignoreAny

ignoreField :: Maybe Q.Property -> M.MemoristM Bool
ignoreField (Just _claim) = pure True
ignoreField Nothing = pure False

data Property = Property Bool

forceHome :: Property -> M.MemoristM Bool
forceHome value = value `seq` pure True
