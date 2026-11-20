{-# LANGUAGE PolyKinds, DataKinds #-}
module StockIndexed where
import qualified Memorist.Memorist as M

data IndexedD (a :: k) = IndexedD (M.ThD Int) deriving (Eq, Show)
same :: IndexedD 'True -> IndexedD 'True -> Bool
same = (==)
render :: IndexedD Int -> String
render = show
