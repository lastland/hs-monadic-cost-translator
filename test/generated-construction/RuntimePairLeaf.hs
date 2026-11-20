module RuntimePairLeaf where

import qualified Memorist.Memorist as M

data BoxD a b = BoxD (M.ThD (M.PairD a b)) deriving (Eq, Show)

same :: BoxD Int Bool -> BoxD Int Bool -> Bool
same = (==)

different :: BoxD Bool Int -> BoxD Bool Int -> Bool
different = (/=)

render :: BoxD Int Bool -> String
render = show

renderAt :: Int -> BoxD Bool Int -> ShowS
renderAt = showsPrec

renderMany :: [BoxD Int Bool] -> ShowS
renderMany = showList
