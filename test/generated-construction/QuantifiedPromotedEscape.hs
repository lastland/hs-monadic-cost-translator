{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, PolyKinds, RankNTypes,
    ScopedTypeVariables, TypeFamilies #-}
module QuantifiedPromotedEscape where
import Data.Proxy
import qualified Memorist.Memorist as M
import QuantifiedPromoted (Index(..), ReprK, ReprApp)
import QuantifiedPromotedDemand (DemandK, DemandApp, DemOf)

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

open :: forall f. ReprK f => f 'Key -> M.MemoristM (M.Rep (f 'Key))
open x = withConstraint (Proxy :: Proxy (ReprApp f 'Key)) (M.reprM x)

observe :: forall f. ReprK f => M.Rep (f 'Key) -> f 'Key
observe x = withConstraint (Proxy :: Proxy (ReprApp f 'Key)) (M.abstract x)

full :: forall f. DemandK f => f 'Key -> DemOf (f 'Key)
full x = withConstraint (Proxy :: Proxy (DemandApp f 'Key)) (M.fullDemand x)

used :: forall f. DemandK f => f 'Key -> DemOf (f 'Key) -> M.USet
used x d = withConstraint (Proxy :: Proxy (DemandApp f 'Key)) (M.usage x d)
