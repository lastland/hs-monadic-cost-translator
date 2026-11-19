{-# LANGUAGE ScopedTypeVariables #-}
module SchemaChangedConstructor where

import qualified Data.Void as Void
import qualified Memorist.Memorist as M
import SchemaViewControl (Tree(..))
import StructuralViewControl (preserve)

claimed :: forall a mapped. (a -> mapped) -> Tree a -> Tree mapped
claimed convert input = case input of
    Leaf fixed field -> Leaf fixed (preserve (\value -> case value of
        M.NothingT -> M.NothingT
        M.JustT payload -> M.JustT (preserve convert payload)) field)
    Fork left right -> error "discarded constructor and fields"

view :: M.Th (Tree Void.Void) -> M.Th (Tree Int)
view input = preserve (claimed Void.absurd) input
