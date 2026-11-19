module ViewRawPattern where

import StructuralViewControl (preserve)
import qualified Memorist.Memorist as M

hidden :: M.Th Int -> M.Th Int
hidden input = let unused = case input of M.Th _ value _ -> value in preserve id input
