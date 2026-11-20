{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, KindSignatures,
    TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module RenamedUtility where
import Data.Kind
import qualified Memorist.Memorist as M

type family NeedFor (a :: Type) :: Constraint where
    NeedFor a = M.Demand a
class NeedFor (f a) => Permit (f :: Type -> Type) (a :: Type)
instance NeedFor (f a) => Permit f a
newtype Wrapped f a = Wrapped { field :: M.Th (f a) }
instance Permit f a => M.Demand (Wrapped f a) where
    type Dem (Wrapped f a) = M.ThD (M.Dem (f a))
    fullDemand (Wrapped x) = M.fullDemand x
    usage (Wrapped x) dx = M.usage x dx

-- Concrete selected home Demand with a first-order context.
data Concrete = Concrete (Wrapped Maybe Int)
instance M.Demand Concrete where
    type Dem Concrete = M.ThD (Maybe Int)
    fullDemand (Concrete x) = M.fullDemand x
    usage (Concrete x) dx = M.usage x dx
