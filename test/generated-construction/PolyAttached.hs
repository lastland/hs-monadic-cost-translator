{-# LANGUAGE PolyKinds, FlexibleContexts, UndecidableInstances, DataKinds #-}
module PolyAttached where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: k -> Type) (a :: k) = HigherD (M.ThD (f a))
  deriving (Eq, Show)
sameInt :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
sameInt = (==)
sameBool :: HigherD M.MaybeD Bool -> HigherD M.MaybeD Bool -> Bool
sameBool = (/=)
renderInt :: HigherD M.MaybeD Int -> String
renderInt = show
renderBool :: HigherD M.MaybeD Bool -> String
renderBool = show

data IndexedD (a :: k) = I (M.ThD Int) deriving (Eq, Show)
indexEq :: IndexedD 'True -> IndexedD 'True -> Bool
indexEq = (==)
indexShow :: IndexedD 'False -> String
indexShow = show
typeIndexEq :: IndexedD Maybe -> IndexedD Maybe -> Bool
typeIndexEq = (==)
typeIndexShow :: IndexedD Int -> String
typeIndexShow = show

data PhantomD (f :: Type -> Type) a = P (M.ThD Int) deriving (Eq, Show)
phantomEq :: PhantomD M.MaybeD 'True -> PhantomD M.MaybeD 'True -> Bool
phantomEq = (==)
phantomShow :: PhantomD M.MaybeD 'False -> String
phantomShow = show
