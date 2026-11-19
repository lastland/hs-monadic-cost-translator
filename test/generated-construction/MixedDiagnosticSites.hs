module MixedDiagnosticSites where

import HiddenConstruction ()
import qualified Test.QuickCheck as Q

draw :: Q.Gen Int
draw = Q.elements
    [1 ..
     3]

steps :: [Int]
steps =
    [1 ..
     3]
