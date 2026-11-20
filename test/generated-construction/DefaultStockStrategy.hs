{-# LANGUAGE PolyKinds, DataKinds, DeriveAnyClass #-}
module DefaultStockStrategy where
import qualified Memorist.Memorist as M

data DefaultD (a :: k) = D (M.ThD Int) deriving (Eq, Show)
defaultEq :: DefaultD 'True -> DefaultD 'True -> Bool
defaultEq = (==)
defaultShow :: DefaultD 'False -> String
defaultShow = show
