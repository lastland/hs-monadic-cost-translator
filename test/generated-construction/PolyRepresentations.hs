{-# LANGUAGE PolyKinds, DataKinds, ConstraintKinds, MagicHash #-}
module PolyRepresentations where
import Data.Kind (Constraint)
import GHC.Exts (TYPE, Int#)
import qualified Memorist.Memorist as M

data ConstraintD (a :: Constraint) = C (M.ThD Int) deriving (Eq, Show)
constraintEq :: ConstraintD (Eq Int) -> ConstraintD (Eq Int) -> Bool
constraintEq = (==)
constraintShow :: ConstraintD (Show Bool) -> String
constraintShow = show

data RepresentationD (a :: TYPE r) = R (M.ThD Int) deriving (Eq, Show)
representationEq :: RepresentationD Int# -> RepresentationD Int# -> Bool
representationEq = (==)
representationShow :: RepresentationD Int -> String
representationShow = show
