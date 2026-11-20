module DemandScalarUnsafe where

import qualified Memorist.Memorist as M

data Observing = Observing (M.Th Int)
instance Eq Observing where
    Observing x == Observing y = M.tval x == M.tval y
instance Show Observing where
    show (Observing x) = show (M.tval x)

customSame :: M.ThD Observing -> M.ThD Observing -> Bool
customSame = (==)
customShow :: M.ThD Observing -> String
customShow = show

protectedSame :: M.ThD (M.Th Int) -> M.ThD (M.Th Int) -> Bool
protectedSame = (==)
protectedShow :: M.ThD (M.Th Int) -> String
protectedShow = show

givenSame :: Eq a => M.ThD a -> M.ThD a -> Bool
givenSame = (==)
givenShow :: Show a => M.ThD a -> String
givenShow = show

data Manual = Manual (M.ThD Int)
instance Eq Manual where
    Manual x == Manual y = x == y
instance Show Manual where
    show (Manual x) = show x
manualSame :: Manual -> Manual -> Bool
manualSame = (==)
manualShow :: Manual -> String
manualShow = show

annotationShow :: M.Annot -> String
annotationShow = show

accountingSame :: M.USet -> M.USet -> Bool
accountingSame = (==)
