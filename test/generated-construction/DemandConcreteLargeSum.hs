module DemandConcreteLargeSum where

import qualified Memorist.Memorist as M

data ManyD = A | B | C | D | E | F | G | H | I | J | Payload (M.ThD Int)
    deriving (Eq, Show)

same :: ManyD -> ManyD -> Bool
same = (==)

different :: ManyD -> ManyD -> Bool
different = (/=)

render :: ManyD -> String
render = show

prec :: Int -> ManyD -> ShowS
prec = showsPrec

list :: [ManyD] -> ShowS
list = showList
