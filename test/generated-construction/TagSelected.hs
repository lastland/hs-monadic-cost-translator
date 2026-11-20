{-# LANGUAGE MagicHash #-}
module TagSelected where

import GHC.Exts (Int#)
import GHC.Magic (dataToTag#)
import qualified Memorist.Memorist as M

data Choice a = This (M.Th a) | That (M.Th a)
data Higher f a = Here (f a) | There (M.Th a)

concrete :: Choice Int -> Int#
concrete = dataToTag#

generic :: Choice a -> Int#
generic = dataToTag#

higher :: Higher f a -> Int#
higher = dataToTag#

type HomeAlias a = Choice a

homeAlias :: HomeAlias a -> Int#
homeAlias = dataToTag#

-- Adjacent constructor counts straddle the pointer-tag boundary on 64-bit
-- targets. On other targets the exact compiler-selected primitive still wins.
data Seven a = S0 | S1 | S2 | S3 | S4 | S5 | S6 (M.Th a)
data Eight a = E0 | E1 | E2 | E3 | E4 | E5 | E6 | E7 (M.Th a)

seven :: Seven a -> Int#
seven = dataToTag#

eight :: Eight a -> Int#
eight = dataToTag#
