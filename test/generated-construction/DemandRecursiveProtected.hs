module DemandRecursiveProtected where

import qualified Memorist.Memorist as M

-- The recursive edge must not hide the protected sibling constructor.
data IndexD = ZeroD | NextD (M.ThD IndexD) | HiddenD (M.ThD (M.Th Int))
    deriving (Eq, Show)

same :: IndexD -> IndexD -> Bool
same = (==)

render :: IndexD -> String
render = show
