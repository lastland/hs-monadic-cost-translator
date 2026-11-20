module DemandConcreteRecursive where

import qualified Memorist.Memorist as M

data IndexD = ZeroD | NextD (M.ThD IndexD) (M.ThD IndexD) deriving (Eq, Show)

same :: IndexD -> IndexD -> Bool
same = (==)

different :: IndexD -> IndexD -> Bool
different = (/=)

render :: IndexD -> String
render = show

prec :: Int -> IndexD -> ShowS
prec = showsPrec

list :: [IndexD] -> ShowS
list = showList

sameThunk :: M.ThD IndexD -> M.ThD IndexD -> Bool
sameThunk = (==)

differentThunk :: M.ThD IndexD -> M.ThD IndexD -> Bool
differentThunk = (/=)

renderThunk :: M.ThD IndexD -> String
renderThunk = show

precThunk :: Int -> M.ThD IndexD -> ShowS
precThunk = showsPrec

listThunk :: [M.ThD IndexD] -> ShowS
listThunk = showList
