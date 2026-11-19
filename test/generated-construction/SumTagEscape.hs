{-# LANGUAGE MagicHash #-}
module SumTagEscape where

import GHC.Exts (Int(I#), dataToTag#)
import SumGeneratorControl (Observation(..))

ordinary :: Observation -> Int
ordinary input = I# (dataToTag# input)
