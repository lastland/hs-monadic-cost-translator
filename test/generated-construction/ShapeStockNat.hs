{-# LANGUAGE DataKinds, FlexibleContexts, KindSignatures, StandaloneDeriving,
    UndecidableInstances #-}
module ShapeStockNat where
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import qualified Memorist.Memorist as M
import ShapeStock (ShapeK)

data NumericD (f :: Nat -> Type) = NumericD (M.ThD (f 0))
deriving instance ShapeK Eq f => Eq (NumericD f)
deriving instance ShapeK Show f => Show (NumericD f)

data TagD (i :: Nat) = TagD Int deriving (Eq, Show)

same :: NumericD TagD -> NumericD TagD -> Bool
same = (==)
render :: NumericD TagD -> String
render = show
