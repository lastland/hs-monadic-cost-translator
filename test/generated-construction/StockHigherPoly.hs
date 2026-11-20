{-# LANGUAGE PolyKinds, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module StockHigherPoly where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: k -> Type) (a :: k) = HigherD (M.ThD (f a))
deriving instance Eq (f a) => Eq (HigherD f a)
deriving instance Show (f a) => Show (HigherD f a)
sameInt :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
sameInt = (==)
renderBool :: HigherD M.MaybeD Bool -> String
renderBool = show

data PhantomD (f :: Type -> Type) a = PhantomD Int deriving (Eq, Show)
phantomEq :: PhantomD M.MaybeD Int -> PhantomD M.MaybeD Int -> Bool
phantomEq = (==)
phantomShow :: PhantomD M.MaybeD Bool -> String
phantomShow = show

data BinaryD (f :: Type -> Type -> Type) (a :: Type) = BinaryD Int deriving (Eq, Show)
binaryEq :: BinaryD Either Int -> BinaryD Either Int -> Bool
binaryEq = (==)
binaryShow :: BinaryD Either Bool -> String
binaryShow = show
