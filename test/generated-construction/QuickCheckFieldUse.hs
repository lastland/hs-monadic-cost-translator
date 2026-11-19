module QuickCheckFieldUse where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

data Cell a = Cell { item :: a }

probe :: Cell (Q.Gen Bool) -> M.MemoristM Bool
probe c = item c `seq` pure True

ignoreItem :: Cell (Q.Gen Bool) -> M.MemoristM Bool
ignoreItem Cell{item = _} = pure True

readBool :: Cell Bool -> Bool
readBool c = item c

prop_probe :: M.Th Bool -> Q.Property
prop_probe t = Q.property (M.value (M.eval1 (probe (Cell (M.tval t `seq` Q.arbitrary)))))
