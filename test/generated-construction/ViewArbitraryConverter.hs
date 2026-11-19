module ViewArbitraryConverter where

import StructuralViewControl (preserve)
import qualified Memorist.Memorist as M

changed :: M.Th Int -> M.Th Int
changed input = preserve (\value -> value + 1) input

callback :: (a -> b) -> M.Th a -> M.Th b
callback transform input = preserve transform input
