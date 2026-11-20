{-# LANGUAGE PolyKinds, FlexibleContexts, UndecidableInstances #-}
module PolyEscapes where
import Data.Kind (Type)
import qualified Memorist.Memorist as M
import PolyAttached (HigherD)

openEq :: Eq (f a) => HigherD f a -> HigherD f a -> Bool
openEq = (==)
openShow :: Show (f a) => HigherD f a -> String
openShow = show
givenEq :: Eq (HigherD f a) => HigherD f a -> HigherD f a -> Bool
givenEq = (==)
givenShow :: Show (HigherD f a) => HigherD f a -> String
givenShow = show
sameClassHelper :: Eq (f a) => f a -> f a -> Bool
sameClassHelper = (==)
showClassHelper :: Show (f a) => f a -> String
showClassHelper = show
deadHelper :: Eq (f a) => f a -> Bool
deadHelper x = let unused = x == x in True
deadSibling :: Eq (f a) => f a -> Bool
deadSibling x = let (kept, unused) = (True, x == x) in kept
protectedEq :: HigherD M.Th Int -> HigherD M.Th Int -> Bool
protectedEq = (==)
protectedShow :: HigherD M.Th Int -> String
protectedShow = show
unknownContainerEq :: HigherD [] Int -> HigherD [] Int -> Bool
unknownContainerEq = (==)
unknownContainerShow :: HigherD [] Int -> String
unknownContainerShow = show
data Observing a = Observing (M.Th a)
instance Eq a => Eq (Observing a) where
    Observing x == Observing y = M.tval x == M.tval y
instance Show a => Show (Observing a) where
    showsPrec n (Observing x) = showsPrec n (M.tval x)
observingEq :: HigherD Observing Int -> HigherD Observing Int -> Bool
observingEq = (==)
observingShow :: HigherD Observing Bool -> String
observingShow = show
data ThFieldD (f :: k -> Type) (a :: k) = ThFieldD (M.Th (f a))
    deriving (Eq, Show)
thFieldEq :: ThFieldD M.MaybeD Int -> ThFieldD M.MaybeD Int -> Bool
thFieldEq = (==)
thFieldShow :: ThFieldD M.MaybeD Bool -> String
thFieldShow = show
data AnnotFieldD (f :: k -> Type) (a :: k)
    = AnnotFieldD (M.ThD (f a)) (M.ThD M.Annot)
    deriving (Eq, Show)
annotFieldEq :: AnnotFieldD M.MaybeD Int -> AnnotFieldD M.MaybeD Int -> Bool
annotFieldEq = (==)
annotFieldShow :: AnnotFieldD M.MaybeD Bool -> String
annotFieldShow = show
class FirstOrder (f :: k -> Type) where
    firstOrder :: f a -> Bool
ordinaryClass :: FirstOrder f => f a -> Bool
ordinaryClass = firstOrder
