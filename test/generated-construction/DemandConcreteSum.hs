module DemandConcreteSum where

import qualified Memorist.Memorist as M

data ChoiceD = EmptyD | IntD (M.ThD Int) | BoolD Bool deriving (Eq, Show)

same :: ChoiceD -> ChoiceD -> Bool
same = (==)

different :: ChoiceD -> ChoiceD -> Bool
different = (/=)

render :: ChoiceD -> String
render = show

prec :: Int -> ChoiceD -> ShowS
prec = showsPrec

list :: [ChoiceD] -> ShowS
list = showList
