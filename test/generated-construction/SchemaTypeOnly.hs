module SchemaTypeOnly where

import qualified Data.Void as Void
import qualified Memorist.Memorist as M
import SchemaViewControl (Tree)
import StructuralViewControl (preserve)

claimed :: (a -> b) -> Tree a -> Tree b
claimed = undefined

view :: M.Th (Tree Void.Void) -> M.Th (Tree Int)
view input = preserve (claimed Void.absurd) input
