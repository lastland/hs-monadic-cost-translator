{-# LANGUAGE ScopedTypeVariables #-}
module SchemaDeadHelper where

import qualified Memorist.Memorist as M
import SchemaViewControl (Phantom(..))

claimed :: forall a mapped. (a -> mapped) -> Phantom a -> Phantom mapped
claimed convert input = let unused = M.tval (error "hidden" :: M.Th Int) in
    case input of Phantom field -> Phantom field
