{-# LANGUAGE ConstraintKinds, GADTs #-}
module DemandScalarForged where

import qualified Memorist.Memorist as M
import Unsafe.Coerce (unsafeCoerce)

data Dict c where
    Dict :: c => Dict c

fake :: Dict (Eq (M.ThD Int))
fake = unsafeCoerce (Dict :: Dict (Eq Int))

forged :: M.ThD Int -> M.ThD Int -> Bool
forged x y = case fake of Dict -> x == y
