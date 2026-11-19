{-# LANGUAGE BangPatterns #-}
module NestedPatternConsumption where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

forced :: M.Th Bool -> Q.Property
forced t = let !_ = M.tval t in Q.property True
