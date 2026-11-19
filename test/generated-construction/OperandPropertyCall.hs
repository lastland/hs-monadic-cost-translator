{-# LANGUAGE TypeApplications #-}
module OperandPropertyCall where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

strict :: a -> M.MemoristM Bool
strict x = x `seq` pure True

prop_direct :: M.Th Bool -> Q.Property
prop_direct x = Q.property (M.value (M.eval1
    (strict @(Q.Gen Bool) (M.tval x `seq` Q.arbitrary))))
