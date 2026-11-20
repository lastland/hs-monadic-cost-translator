module StockResetCompound where

import qualified Memorist.Memorist as M

data MixedD a b = MixedD (M.ThD (MixedD a (M.ThD Bool))) (M.ThD a) (M.ThD b)
    deriving (Eq, Show)

sameMixed :: MixedD Int Char -> MixedD Int Char -> Bool
sameMixed = (==)
renderMixed :: MixedD Int Char -> String
renderMixed = show
