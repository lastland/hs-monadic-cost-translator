{-# LANGUAGE ScopedTypeVariables #-}
module SchemaIndependentFields where

import qualified Data.Void as Void
import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)

data Twin a = Twin (M.Th Int) (M.Th Int) (M.Th a)

claimed :: forall a mapped. (a -> mapped) -> Twin a -> Twin mapped
claimed convert input = case input of
    Twin first second field -> Twin second first (preserve convert field)

view :: M.Th (Twin Void.Void) -> M.Th (Twin Int)
view input = preserve (claimed Void.absurd) input
