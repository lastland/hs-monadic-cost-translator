{-# LANGUAGE DeriveAnyClass #-}
module DemandScalarShadow where

import Prelude hiding (Eq, Show)
import qualified Memorist.Memorist as M

class Eq a where
    same :: a -> Bool
    same _ = True
class Show a where
    render :: a -> String
    render _ = ""
data BoxD = BoxD (M.ThD Int) deriving (Eq, Show)
shadowSame :: BoxD -> Bool
shadowSame = same
shadowShow :: BoxD -> String
shadowShow = render
