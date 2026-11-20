{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    PolyKinds, StandaloneDeriving, StandaloneKindSignatures, TypeFamilies,
    UndecidableInstances, UndecidableSuperClasses #-}
module StockRenamed where

import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type Requirement :: (Type -> Constraint) -> forall k -> k -> Constraint
type family Requirement c k a where
    Requirement c Type a = c a
class Requirement c Type a => Permit c a
instance Requirement c Type a => Permit c a

data LabelledD a = LabelledD { labelD :: M.ThD a }
deriving instance Permit Eq a => Eq (LabelledD a)
deriving instance Permit Show a => Show (LabelledD a)

same :: LabelledD Int -> LabelledD Int -> Bool
same = (==)
render :: LabelledD Bool -> String
render = show
