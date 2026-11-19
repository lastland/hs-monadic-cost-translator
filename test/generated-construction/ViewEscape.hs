module ViewEscape where

import StructuralViewControl (preserve, spineView)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

firstClass :: (a -> b) -> M.Th a -> M.Th b
firstClass = preserve

partial :: M.Th Int -> M.Th Int
partial = preserve id

listEscape :: (a -> b) -> L.ListT a -> L.ListT b
listEscape = spineView
