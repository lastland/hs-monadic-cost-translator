{-# LANGUAGE TypeFamilies #-}
module LexicalFamilyEvidence where

type family Result a
type family Opaque a

-- This scope does not have the downstream Int instance. Its equality is a
-- condition on this function, not an unconditional fact about Result ().
conditional :: Result () ~ (Int -> Int) => Result () -> Int
conditional f = f 3

conditionalData :: Opaque () ~ Bool => Opaque () -> Bool
conditionalData x = x
