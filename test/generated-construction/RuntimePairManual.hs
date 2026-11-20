module RuntimePairManual where

import qualified Memorist.Memorist as M

newtype Bad = Bad Int
instance Eq Bad where
    Bad x == Bad y = x == y
instance Show Bad where
    showsPrec n (Bad x) = showsPrec n x

data LeftD = LeftD (M.ThD (M.PairD Bad Bool)) deriving (Eq, Show)
data RightD = RightD (M.ThD (M.PairD Int Bad)) deriving (Eq, Show)

leftEq :: LeftD -> LeftD -> Bool
leftEq = (==)
rightEq :: RightD -> RightD -> Bool
rightEq = (==)
leftShow :: LeftD -> String
leftShow = show
rightShow :: RightD -> String
rightShow = show
