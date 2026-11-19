{-# LANGUAGE ConstraintKinds, KindSignatures, TypeFamilies #-}
module ConstraintWrapperWrongProtocol where

import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M

withConstraint :: Proxy (c :: Constraint) -> r -> r
withConstraint _ body = body

data Source = Source Int
data Target = Target (M.Th Int)

instance M.Repr Source where
    type Rep Source = Target
    reprM (Source input) = Target <$> M.lazy (M.reprM input)
    abstract (Target field) = withConstraint (Proxy :: Proxy (M.Repr Int)) (Source (M.abstract (M.tval field)))
