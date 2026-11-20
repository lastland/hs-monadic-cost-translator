module RuntimePairProtectedAnnotated where

import qualified Memorist.Memorist as M

data LeftD = LeftD (M.ThD (M.PairD (M.Annotated Int) Bool)) deriving (Eq, Show)
data RightD = RightD (M.ThD (M.PairD Int (M.Annotated Bool))) deriving (Eq, Show)

leftEq :: LeftD -> LeftD -> Bool
leftEq = (==)
rightEq :: RightD -> RightD -> Bool
rightEq = (==)
leftShow :: LeftD -> String
leftShow = show
rightShow :: RightD -> String
rightShow = show
