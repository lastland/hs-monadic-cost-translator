{-# LANGUAGE DataKinds, ImpredicativeTypes, KindSignatures, PolyKinds, RankNTypes, TypeApplications #-}
module ForallClosureChanged where

import ForallClosureControl (Indexed(..))
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)

changed :: M.Th (forall {k} (index :: k). Indexed index) -> M.Th (Indexed 'True)
changed input = preserve @(forall {k} (index :: k). Indexed index) (\payload -> Indexed undefined) input

residual :: M.Th (forall b. M.PairT Int (L.ListT b)) -> M.Th (M.PairT Int (L.ListT Bool))
residual input = preserve @(forall b. M.PairT Int (L.ListT b)) (\payload -> M.PairT undefined undefined) input
