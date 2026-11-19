{-# LANGUAGE ImpredicativeTypes, RankNTypes, TypeApplications #-}
module StructuralViewControl where

import qualified Data.Void as Void
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Exported declarations are checked here, not arbitrary future consumers.
preserve :: (a -> b) -> M.Th a -> M.Th b
preserve transform (M.Th identity payload metadata) =
    M.Th identity (transform payload) metadata

spineView :: (a -> b) -> L.ListT a -> L.ListT b
spineView transform input = case input of
    L.NilT -> L.NilT
    L.ConsT first rest -> L.ConsT (preserve transform first) (preserve (spineView transform) rest)

identityView :: M.Th Int -> M.Th Int
identityView input = preserve id input

emptyView :: M.Th (L.ListT Void.Void) -> M.Th (L.ListT Int)
emptyView input = preserve (spineView Void.absurd) input

universalView :: M.Th (forall a. M.MaybeT a) -> M.Th (M.MaybeT Int)
universalView input = preserve @(forall a. M.MaybeT a) (\payload -> payload) input
