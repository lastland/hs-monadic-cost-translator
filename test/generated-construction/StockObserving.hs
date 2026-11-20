{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module StockObserving where

import qualified Memorist.Memorist as M
import StockBox
import StockRecursive

data Observing = Observing (M.Th Int)
instance Eq Observing where
    Observing x == Observing y = M.tval x == M.tval y
instance Show Observing where
    showsPrec n (Observing x) = showsPrec n (M.tval x)

boxObserving :: BoxD Observing -> BoxD Observing -> Bool
boxObserving = (==)
packetObserving :: PacketD Observing -> String
packetObserving = show
recursiveObserving :: ChainD Observing -> String
recursiveObserving = show

data SiblingD a = SiblingD (M.ThD (SiblingD a)) (M.ThD a) (M.ThD Observing)
deriving instance ShapeK Eq a => Eq (SiblingD a)
deriving instance ShapeK Show a => Show (SiblingD a)
siblingEq :: SiblingD Int -> SiblingD Int -> Bool
siblingEq = (==)
siblingShow :: SiblingD Bool -> String
siblingShow = show

protectedBox :: BoxD (M.Th Int) -> BoxD (M.Th Int) -> Bool
protectedBox = (==)
protectedPacket :: PacketD M.Annot -> String
protectedPacket = show
