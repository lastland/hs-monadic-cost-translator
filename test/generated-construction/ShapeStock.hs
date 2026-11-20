{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    PolyKinds, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures,
    TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module ShapeStock where
import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type ShapeReq :: (Type -> Constraint) -> forall k -> k -> Constraint
type family ShapeReq c k a where
    ShapeReq c Type a = c a
    ShapeReq c (k -> l) f = ShapeF c f
    ShapeReq c k a = ()
type ShapeK :: (Type -> Constraint) -> forall k. k -> Constraint
class ShapeReq c k a => ShapeK c (a :: k)
instance ShapeReq c k a => ShapeK c (a :: k)
type ShapeF :: (Type -> Constraint) -> forall k l. (k -> l) -> Constraint
class (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f
instance (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f

data HigherD (f :: Type -> Type) (a :: Type) = HigherD (M.ThD a) (M.ThD (f a))
deriving instance (ShapeK Eq f, ShapeK Eq a) => Eq (HigherD f a)
deriving instance (ShapeK Show f, ShapeK Show a) => Show (HigherD f a)

sameInt :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
sameInt = (==)
differentBool :: HigherD M.MaybeD Bool -> HigherD M.MaybeD Bool -> Bool
differentBool = (/=)
renderInt :: HigherD M.MaybeD Int -> String
renderInt = show
precBool :: Int -> HigherD M.MaybeD Bool -> ShowS
precBool = showsPrec
listInt :: [HigherD M.MaybeD Int] -> ShowS
listInt = showList
