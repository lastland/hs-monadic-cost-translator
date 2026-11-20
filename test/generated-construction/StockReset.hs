module StockReset where

import qualified Memorist.Memorist as M

data ResetD a = ResetD (M.ThD (ResetD Int)) (M.ThD a) deriving (Eq, Show)

sameBool :: ResetD Bool -> ResetD Bool -> Bool
sameBool = (==)
renderBool :: ResetD Bool -> String
renderBool = show
