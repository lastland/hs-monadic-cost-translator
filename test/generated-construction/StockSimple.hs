module StockSimple where
import qualified Memorist.Memorist as M

data BoxD a = BoxD (M.ThD a) deriving (Eq, Show)

sameInt :: BoxD Int -> BoxD Int -> Bool
sameInt = (==)
sameBool :: BoxD Bool -> BoxD Bool -> Bool
sameBool = (==)
renderInt :: BoxD Int -> String
renderInt = show
renderBool :: BoxD Bool -> String
renderBool = show
