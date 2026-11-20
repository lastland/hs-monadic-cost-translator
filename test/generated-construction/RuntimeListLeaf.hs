module RuntimeListLeaf where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data BoxD a = BoxD (M.ThD (L.ListD a)) deriving (Eq, Show)

same :: BoxD Int -> BoxD Int -> Bool
same = (==)

render :: BoxD Bool -> String
render = show
