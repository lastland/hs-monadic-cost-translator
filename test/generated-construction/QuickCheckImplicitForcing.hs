module QuickCheckImplicitForcing where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

ignore :: Q.Gen Bool -> M.MemoristM Bool
ignore _draw = pure True

prop_ignored :: M.Th Bool -> Q.Property
prop_ignored x = Q.property (M.value (M.eval1 (ignore (M.tval x `seq` Q.arbitrary))))

data Box = Box Q.Property

pack :: Q.Property -> Box
pack = Box

prop_packed :: M.Th Bool -> Q.Property
prop_packed x = case pack (M.tval x `seq` Q.property True) of
    Box _ -> Q.property True
