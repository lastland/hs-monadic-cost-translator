module RuntimeDemandCompositions where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data BoxD a b = BoxD
    (M.ThD (L.ListD (M.PairD a b)))
    (M.ThD (M.PairD (L.ListD a) (M.MaybeD b)))
    (M.ThD (M.MaybeD (L.ListD a)))
    (M.ThD (L.ListD (L.ListD b)))
    deriving (Eq, Show)

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
