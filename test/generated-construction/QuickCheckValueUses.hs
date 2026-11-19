{-# LANGUAGE TypeApplications #-}
module QuickCheckValueUses where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

forceDraw :: Q.Gen Bool -> M.MemoristM Bool
forceDraw draw =
    draw `seq` pure True

prop_force :: M.Th Bool -> Q.Property
prop_force x = Q.property (M.value (M.eval1 (forceDraw (M.tval x `seq` Q.arbitrary))))

fromCase :: Maybe Q.Property -> M.MemoristM Bool
fromCase candidate = case candidate of
    Just claim -> claim `seq` pure True
    Nothing -> pure False

first :: (a, b) -> a
first (a, _) = a

fromPair :: (Q.Property, Bool) -> M.MemoristM Bool
fromPair pair = first @Q.Property pair `seq` pure True
