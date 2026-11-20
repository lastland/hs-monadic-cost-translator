{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module ShapeStockSum where
import ShapeStock (ShapeK)
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data SumD (f :: Type -> Type) a
    = NoneD | ScalarD (M.ThD a) | BothD (M.ThD (f a)) (M.ThD (SumD f a))
deriving instance (ShapeK Eq f, ShapeK Eq a) => Eq (SumD f a)
deriving instance (ShapeK Show f, ShapeK Show a) => Show (SumD f a)

sameInt :: SumD M.MaybeD Int -> SumD M.MaybeD Int -> Bool
sameInt = (==)
differentBool :: SumD M.MaybeD Bool -> SumD M.MaybeD Bool -> Bool
differentBool = (/=)
renderInt :: SumD M.MaybeD Int -> String
renderInt = show
precBool :: Int -> SumD M.MaybeD Bool -> ShowS
precBool = showsPrec
listInt :: [SumD M.MaybeD Int] -> ShowS
listInt = showList
