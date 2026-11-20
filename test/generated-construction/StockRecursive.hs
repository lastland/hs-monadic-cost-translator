{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module StockRecursive where

import qualified Memorist.Memorist as M
import StockBox (ShapeK)

data ChainD a = ChainD (M.ThD (ChainD a)) (M.ThD a)
deriving instance ShapeK Eq a => Eq (ChainD a)
deriving instance ShapeK Show a => Show (ChainD a)

sameChain :: ChainD Int -> ChainD Int -> Bool
sameChain = (==)
showChain :: ChainD Bool -> String
showChain = show

data FlipD a b = FlipD (M.ThD (FlipD b a)) (M.ThD a) (M.ThD b)
deriving instance (ShapeK Eq a, ShapeK Eq b) => Eq (FlipD a b)
deriving instance (ShapeK Show a, ShapeK Show b) => Show (FlipD a b)

sameFlip :: FlipD Int Bool -> FlipD Int Bool -> Bool
sameFlip = (==)
showFlip :: FlipD Bool Int -> String
showFlip = show

data EvenD a = EvenD (M.ThD (OddD a)) (M.ThD a)
data OddD a = OddD (M.ThD (EvenD a)) (M.ThD a)
deriving instance ShapeK Eq a => Eq (EvenD a)
deriving instance ShapeK Eq a => Eq (OddD a)
deriving instance ShapeK Show a => Show (EvenD a)
deriving instance ShapeK Show a => Show (OddD a)

sameMutual :: EvenD Int -> EvenD Int -> Bool
sameMutual = (==)
showMutual :: OddD Bool -> String
showMutual = show
