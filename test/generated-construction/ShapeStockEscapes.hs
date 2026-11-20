{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
    TypeApplications, UndecidableInstances, UndecidableSuperClasses #-}
module ShapeStockEscapes where
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M
import ShapeStock (ShapeK, HigherD(..))

givenEq :: (ShapeK Eq f, ShapeK Eq a) => M.ThD (f a) -> M.ThD (f a) -> Bool
givenEq = (==)
givenShow :: (ShapeK Show f, ShapeK Show a) => M.ThD (f a) -> String
givenShow = show
borrowEq :: (ShapeK Eq f, ShapeK Eq a) => HigherD f a -> HigherD f a -> Bool
borrowEq = (==)
borrowShow :: (ShapeK Show f, ShapeK Show a) => HigherD f a -> String
borrowShow = show
deadHelper :: (ShapeK Eq f, ShapeK Eq a) => HigherD f a -> HigherD f a -> Int
deadHelper x y = let unused = x == y in 1

class (ShapeK Eq f, ShapeK Eq a) => Other f a where
    other :: HigherD f a -> HigherD f a -> Bool
    other = (==)

withEvidence :: forall (c :: Constraint) r. c => Proxy c -> (c => r) -> r
withEvidence _ callback = callback
wrapped :: forall f a. (ShapeK Eq f, ShapeK Eq a) => HigherD f a -> HigherD f a -> Bool
wrapped x y = withEvidence (Proxy @(ShapeK Eq f)) (x == y)
deadWrapped :: forall f a. (ShapeK Show f, ShapeK Show a) => HigherD f a -> Int
deadWrapped x = withEvidence (Proxy @(ShapeK Show f)) (let unused = show x in 1)

data Watching a = Watching a
instance Eq a => Eq (Watching a) where
    Watching x == Watching y = x == y
instance Show a => Show (Watching a) where
    showsPrec p (Watching x) = showsPrec p x
observingEq :: HigherD Watching Int -> HigherD Watching Int -> Bool
observingEq = (==)
observingShow :: HigherD Watching Bool -> String
observingShow = show

data ProtectedD (f :: Type -> Type) a = ProtectedD (M.ThD (f a)) (M.Th Int)
deriving instance (ShapeK Eq f, ShapeK Eq a) => Eq (ProtectedD f a)
deriving instance (ShapeK Show f, ShapeK Show a) => Show (ProtectedD f a)
protectedEq :: ProtectedD M.MaybeD Int -> ProtectedD M.MaybeD Int -> Bool
protectedEq = (==)
protectedShow :: ProtectedD M.MaybeD Bool -> String
protectedShow = show
