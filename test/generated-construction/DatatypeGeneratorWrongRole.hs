module DatatypeGeneratorWrongRole where

import DatatypeGeneratorControl (sampleField)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- The same field types do not establish an actual Dem equation for this type.
data Unrelated = Unrelated (M.ThD Int)

instance QC.Arbitrary Unrelated where
    arbitrary = QC.sized (\size -> Unrelated <$> sampleField (size `div` 1))
    shrink _ = []
