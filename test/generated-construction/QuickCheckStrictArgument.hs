{-# LANGUAGE BangPatterns, TypeApplications #-}
module QuickCheckStrictArgument where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

strict :: a -> M.MemoristM Bool
strict x = x `seq` pure True

specialized :: Q.Gen Bool -> M.MemoristM Bool
specialized = strict @(Q.Gen Bool)

section :: Q.Gen Bool -> M.MemoristM Bool
section = (`seq` pure True)

bang :: Q.Property -> M.MemoristM Bool
bang !_ = pure True

prop_specialized :: M.Th Bool -> Q.Property
prop_specialized x = Q.property (M.value (M.eval1 (specialized (M.tval x `seq` Q.arbitrary))))
