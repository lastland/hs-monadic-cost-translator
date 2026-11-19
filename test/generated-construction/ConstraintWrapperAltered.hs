{-# LANGUAGE ConstraintKinds, RankNTypes, TypeFamilies #-}
module ConstraintWrapperAltered where

import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = error "not the identity protocol"

data Source = Source Int
data Target = Target (M.Th Int)

instance M.Repr Source where
    type Rep Source = Target
    reprM (Source input) = Target <$> M.lazy (M.reprM input)
    abstract (Target field) = withConstraint (Proxy :: Proxy (M.Repr Int)) (Source (M.abstract (M.tval field)))
