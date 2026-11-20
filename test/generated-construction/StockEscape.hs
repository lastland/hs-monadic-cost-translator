{-# LANGUAGE FlexibleContexts #-}
module StockEscape where

import qualified Memorist.Memorist as M
import StockBox

ordinaryOpen :: Eq a => a -> a -> Bool
ordinaryOpen = (==)
ordinaryShape :: ShapeK Eq a => a -> a -> Bool
ordinaryShape = (==)
openBox :: ShapeK Eq a => BoxD a -> BoxD a -> Bool
openBox = (==)
openShow :: ShapeK Show a => BoxD a -> String
openShow = show
givenBox :: Eq (BoxD a) => BoxD a -> BoxD a -> Bool
givenBox = (==)
openDemand :: Eq a => M.ThD a -> M.ThD a -> Bool
openDemand = (==)

deadHelper :: Eq a => a -> Bool
deadHelper x = let unused = x == x in True
deadSibling :: Eq a => a -> Bool
deadSibling x = let (kept, unused) = (True, x == x) in kept
