module MappedViewLeak where

import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)
import qualified Test.QuickCheck as Q

law :: M.Th Int -> Q.Property
law x = Q.property (helper x `seq` True)
  where
{-# LINE 50 "SharedViewInput.hs" #-}
    helper v = M.tval      v
