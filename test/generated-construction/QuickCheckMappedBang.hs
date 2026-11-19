{-# LANGUAGE BangPatterns #-}
module QuickCheckMappedBang where

import qualified Test.QuickCheck as Q

ordinary :: Q.Property -> Bool
{-# LINE 20 "OrdinaryBang.hs" #-}
ordinary !_ = True
