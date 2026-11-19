{-# LANGUAGE DataKinds, ImpredicativeTypes, KindSignatures, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module ForallClosureControl where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)

data Indexed (index :: k) = Indexed (M.Th Int)
data Applied (f :: k -> Type) (index :: k) = Applied (M.Th (f index))

implicitKind :: M.Th (forall {k} (index :: k). Indexed index) -> M.Th (Indexed 'True)
implicitKind input = preserve @(forall {k} (index :: k). Indexed index) (\payload -> payload) input

higher :: M.Th (forall (f :: Type -> Type) a. Applied f a) -> M.Th (Applied M.MaybeT Int)
higher input = preserve @(forall (f :: Type -> Type) a. Applied f a) (\payload -> payload) input

indexedHigher :: M.Th (forall {k} (f :: k -> Type) (index :: k). Applied f index) -> M.Th (Applied Proxy 'True)
indexedHigher input = preserve @(forall {k} (f :: k -> Type) (index :: k). Applied f index) (\payload -> payload) input

promoted :: M.Th (forall (index :: Bool). Indexed index) -> M.Th (Indexed 'False)
promoted input = preserve @(forall (index :: Bool). Indexed index) (\payload -> payload) input

-- One ordinary allocation of the exact residual forall closure. The key's
-- original Num contract does not grant a converter or a second helper type.
residualKey :: forall key. Num key => M.Th key -> (forall b. M.PairT Int (L.ListT b))
    -> M.MemoristM (M.Th (M.PairT Int (L.ListT Bool)))
residualKey _ producer = do
    root <- M.lazy @M.MemoristM @(forall b. M.PairT Int (L.ListT b))
        (pure @M.MemoristM @(forall b. M.PairT Int (L.ListT b)) producer)
    pure (preserve @(forall b. M.PairT Int (L.ListT b)) (\payload -> payload) root)

vacuous :: forall k. M.Th (forall (b :: k). M.PairT Int (L.ListT Bool)) -> M.Th (forall (b :: k). M.PairT Int (L.ListT Bool))
vacuous input = input
