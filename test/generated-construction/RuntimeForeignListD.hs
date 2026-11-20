module RuntimeForeignListD where

import qualified Memorist.Memorist as M

data ListD a = ListD a
instance Eq a => Eq (ListD a) where
    ListD x == ListD y = x == y
instance Show a => Show (ListD a) where
    showsPrec n (ListD x) = showsPrec n x
data BoxD a = BoxD (M.ThD (ListD a)) deriving (Eq, Show)

same :: BoxD Int -> BoxD Int -> Bool
same = (==)
render :: BoxD Bool -> String
render = show
