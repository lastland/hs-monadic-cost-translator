{-# LANGUAGE KindSignatures, FlexibleContexts, UndecidableInstances #-}
module StockHigherSum where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: Type -> Type) a
    = LeftD (M.ThD a) (M.ThD (f a))
    | RightD (M.ThD a) (M.ThD (f a))
    deriving (Eq, Show)

sameInt :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
sameInt = (==)
sameBool :: HigherD M.MaybeD Bool -> HigherD M.MaybeD Bool -> Bool
sameBool = (==)
renderInt :: HigherD M.MaybeD Int -> String
renderInt = show
renderBool :: HigherD M.MaybeD Bool -> String
renderBool = show
