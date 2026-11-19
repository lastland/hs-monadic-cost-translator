{-# LANGUAGE RebindableSyntax #-}
module RebindableExpansion where

import Prelude
import qualified Memorist.Memorist as M

ifThenElse :: Bool -> a -> a -> a
ifThenElse condition yes no = case condition of
    True -> yes
    False -> no

ordinary :: M.MemoristM Int
ordinary = if True then pure 17 else pure 18
