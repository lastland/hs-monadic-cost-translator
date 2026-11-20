{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    PolyKinds, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures,
    TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module RenamedStock where
import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type Needs :: (Type -> Constraint) -> forall k -> k -> Constraint
type family Needs c k a where
    Needs c Type a = c a
    Needs c (k -> l) f = Maps c f
    Needs c k a = ()
type Carries :: (Type -> Constraint) -> forall k. k -> Constraint
class Needs c k a => Carries c (a :: k)
instance Needs c k a => Carries c (a :: k)
type Maps :: (Type -> Constraint) -> forall k l. (k -> l) -> Constraint
class (forall a. Carries c a => Carries c (f a)) => Maps c f
instance (forall a. Carries c a => Carries c (f a)) => Maps c f
data RenamedD (f :: Type -> Type) (a :: Type) = RenamedD (M.ThD (f a))
deriving instance (Carries Eq f, Carries Eq a) => Eq (RenamedD f a)
deriving instance (Carries Show f, Carries Show a) => Show (RenamedD f a)

same :: RenamedD M.MaybeD Int -> RenamedD M.MaybeD Int -> Bool
same = (==)
different :: RenamedD M.MaybeD Bool -> RenamedD M.MaybeD Bool -> Bool
different = (/=)
render :: RenamedD M.MaybeD Bool -> String
render = show
