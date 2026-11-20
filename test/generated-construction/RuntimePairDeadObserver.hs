module RuntimePairDeadObserver where

import qualified Memorist.Memorist as M

data Bad = Bad (M.Th Int)
instance Eq Bad where
    _ == _ = True
instance Show Bad where
    showsPrec _ _ = id

dead :: Bad -> Int
dead (Bad x) = M.tval x

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
