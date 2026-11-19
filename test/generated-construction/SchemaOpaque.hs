{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}
module SchemaOpaque where

import Data.Kind (Type)
import qualified Data.Void as Void
import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)

data Higher (f :: Type -> Type) a = Higher (M.Th (f a))

claimed :: forall f a mapped. Functor f => (a -> mapped) -> Higher f a -> Higher f mapped
claimed convert input = case input of
    Higher field -> Higher (preserve (fmap convert) field)

view :: Functor f => M.Th (Higher f Void.Void) -> M.Th (Higher f Int)
view input = preserve (claimed Void.absurd) input
