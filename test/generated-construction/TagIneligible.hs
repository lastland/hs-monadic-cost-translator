{-# LANGUAGE FlexibleContexts, MagicHash #-}
module TagIneligible where

import GHC.Exts (Int#)
import GHC.Magic (DataToTag, dataToTag#)
import qualified Memorist.Memorist as M

type RawAlias a = M.Th a

raw :: RawAlias a -> Int#
raw = dataToTag#

foreignOperand :: Maybe a -> Int#
foreignOperand = dataToTag#

given :: DataToTag a => a -> Int#
given = dataToTag#
