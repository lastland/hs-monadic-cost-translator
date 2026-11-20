{-# LANGUAGE ConstraintKinds, GADTs, KindSignatures, MagicHash #-}
module TagForged where

import Data.Kind (Constraint)
import GHC.Exts (Int(I#), Int#)
import GHC.Magic (DataToTag, dataToTag#)
import GHC.Prim (dataToTagLarge#)
import Unsafe.Coerce (unsafeCoerce)

data IndexD = ZeroD | NextD Int
data Dict (c :: Constraint) where
    Dict :: c => Dict c

-- A large-family primitive cast to a dictionary is not builtin evidence.
wrongPrimop :: IndexD -> Int
wrongPrimop input =
    case unsafeCoerce (dataToTagLarge# :: IndexD -> Int#) :: Dict (DataToTag IndexD) of
        Dict -> I# (dataToTag# input)

-- Nor is an arbitrary coerced function.
wrongCast :: IndexD -> Int
wrongCast input =
    case unsafeCoerce (const 0 :: IndexD -> Int) :: Dict (DataToTag IndexD) of
        Dict -> I# (dataToTag# input)
