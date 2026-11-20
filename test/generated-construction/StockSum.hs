module StockSum where
import qualified Memorist.Memorist as M

data ChoiceD a = LeftD (M.ThD a) | RightD (M.ThD a) deriving (Eq, Show)

sameInt :: ChoiceD Int -> ChoiceD Int -> Bool
sameInt = (==)
sameBool :: ChoiceD Bool -> ChoiceD Bool -> Bool
sameBool = (==)
renderInt :: ChoiceD Int -> String
renderInt = show
renderBool :: ChoiceD Bool -> String
renderBool = show
