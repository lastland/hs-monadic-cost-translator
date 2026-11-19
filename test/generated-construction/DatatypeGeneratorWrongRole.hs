module DatatypeGeneratorWrongRole where

import QuickCheckRoles (sampleField)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- An Arbitrary generator does not need a corresponding Dem equation.
data Unrelated = Unrelated (M.ThD Int)

instance QC.Arbitrary Unrelated where
    arbitrary = QC.sized (\size -> Unrelated <$> sampleField (size `div` 1))
    shrink _ = []
