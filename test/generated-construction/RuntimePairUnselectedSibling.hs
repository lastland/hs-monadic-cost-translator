module RuntimePairUnselectedSibling where

import qualified Memorist.Memorist as M

data Bad = Bad (M.Th Int)
instance Eq Bad where
    _ == _ = True
-- No caller selects Show, but the raw method and its defaults remain checked.
instance Show Bad where
    showsPrec n (Bad x) = showsPrec n (M.tval x)

data LeftD = LeftD (M.ThD (M.PairD Bad Bool)) deriving (Eq, Show)
data RightD = RightD (M.ThD (M.PairD Int Bad)) deriving (Eq, Show)

leftEq :: LeftD -> LeftD -> Bool
leftEq = (==)
rightEq :: RightD -> RightD -> Bool
rightEq = (==)
