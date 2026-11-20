module RuntimePreludeList where

import qualified Memorist.Memorist as M

data BoxD a = BoxD (M.ThD [a]) deriving (Eq, Show)

same :: BoxD Int -> BoxD Int -> Bool
same = (==)
render :: BoxD Bool -> String
render = show
