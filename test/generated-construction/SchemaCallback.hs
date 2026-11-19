module SchemaCallback where

import qualified Memorist.Memorist as M
import SchemaViewControl (Tree, branch)
import StructuralViewControl (preserve)

view :: (a -> b) -> M.Th (Tree a) -> M.Th (Tree b)
view callback input = preserve (branch callback) input
