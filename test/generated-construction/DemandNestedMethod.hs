module DemandNestedMethod where

import qualified Memorist.Memorist as M

same :: M.ThD (M.ThD Int) -> M.ThD (M.ThD Int) -> Bool
same = (==)

render :: M.ThD (M.ThD Int) -> String
render = show
