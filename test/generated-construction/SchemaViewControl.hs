{-# LANGUAGE ScopedTypeVariables #-}
module SchemaViewControl where

import qualified Data.Void as Void
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import StructuralViewControl (preserve, spineView)

data Tree a = Leaf (M.Th Int) (M.Th (M.MaybeT a)) | Fork (M.Th (Tree a)) (M.Th (Tree a))
data Nest a = End (M.Th Int) (M.Th (M.MaybeT a)) | More (M.Th (Nest (L.ListT a)))
data Box a b = Box (M.Th Int) (M.Th (M.MaybeT a)) (M.Th (L.ListT b))
data Phantom a = Phantom (M.Th Int)
data Mixed a = Mixed (M.Th (M.PairT Int a))

-- These names deliberately carry no map/type/slot naming convention.
branch :: forall a mapped. (a -> mapped) -> Tree a -> Tree mapped
branch convert input = case input of
    Leaf fixed field -> Leaf fixed (preserve (\value -> case value of
        M.NothingT -> M.NothingT
        M.JustT payload -> M.JustT (preserve convert payload)) field)
    Fork left right -> Fork (preserve (branch convert) left) (preserve (branch convert) right)

level :: forall a mapped. (a -> mapped) -> Nest a -> Nest mapped
level convert input = case input of
    End fixed field -> End fixed (preserve (\value -> case value of
        M.NothingT -> M.NothingT
        M.JustT payload -> M.JustT (preserve convert payload)) field)
    More rest -> More (preserve (level (spineView convert)) rest)

leftSlot :: forall a b mapped. (a -> mapped) -> Box a b -> Box mapped b
leftSlot convert input = case input of
    Box fixed left right -> Box fixed (preserve (\value -> case value of
        M.NothingT -> M.NothingT
        M.JustT payload -> M.JustT (preserve convert payload)) left) right

rightSlot :: forall a b mapped. (b -> mapped) -> Box a b -> Box a mapped
rightSlot convert input = case input of
    Box fixed left right -> Box fixed left (preserve (spineView convert) right)

phantomSlot :: forall a mapped. (a -> mapped) -> Phantom a -> Phantom mapped
phantomSlot convert input = case input of Phantom fixed -> Phantom fixed

pairSlot :: forall a mapped. (a -> mapped) -> Mixed a -> Mixed mapped
pairSlot convert input = case input of
    Mixed field -> Mixed (preserve (\value -> case value of
        M.PairT fixed varied -> M.PairT fixed (preserve convert varied)) field)

treeView :: M.Th (Tree Void.Void) -> M.Th (Tree Int)
treeView input = preserve (branch Void.absurd) input

nestView :: M.Th (Nest Void.Void) -> M.Th (Nest Bool)
nestView input = preserve (level Void.absurd) input

bothView :: M.Th (Box Void.Void Void.Void) -> M.Th (Box Int Bool)
bothView input = preserve (\value -> rightSlot Void.absurd (leftSlot Void.absurd value)) input

phantomView :: M.Th (Phantom Void.Void) -> M.Th (Phantom Int)
phantomView input = preserve (phantomSlot Void.absurd) input

pairView :: M.Th (Mixed Void.Void) -> M.Th (Mixed Int)
pairView input = preserve (pairSlot Void.absurd) input
