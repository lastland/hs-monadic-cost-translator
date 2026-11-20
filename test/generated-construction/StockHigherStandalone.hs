{-# LANGUAGE KindSignatures, ExplicitForAll, FlexibleContexts, StandaloneDeriving,
    DerivingStrategies, UndecidableInstances #-}
module StockHigherStandalone where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: Type -> Type) a = HigherD (M.ThD a) (M.ThD (f a))
deriving stock instance forall (f :: Type -> Type) (a :: Type).
    (Eq a, Eq (f a)) => Eq (HigherD f a)
deriving stock instance forall (f :: Type -> Type) (a :: Type).
    (Show a, Show (f a)) => Show (HigherD f a)

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

data PhantomD (f :: Type -> Type) (a :: Type) = PhantomD Int deriving (Eq, Show)
phantomEq :: PhantomD M.MaybeD Int -> PhantomD M.MaybeD Int -> Bool
phantomEq = (==)
phantomShow :: PhantomD M.MaybeD Bool -> String
phantomShow = show
