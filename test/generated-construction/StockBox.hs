{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    PolyKinds, StandaloneDeriving, StandaloneKindSignatures, TypeFamilies,
    UndecidableInstances, UndecidableSuperClasses #-}
module StockBox where

import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type ShapeReq :: (Type -> Constraint) -> forall k -> k -> Constraint
type family ShapeReq c k a where
    ShapeReq c Type a = c a
class ShapeReq c Type a => ShapeK c a
instance ShapeReq c Type a => ShapeK c a

data BoxD a = BoxD { itemD :: M.ThD a, countD :: M.ThD Int }
deriving instance ShapeK Eq a => Eq (BoxD a)
deriving instance ShapeK Show a => Show (BoxD a)

data PacketD a = PacketD (M.ThD (M.MaybeD a))
deriving instance ShapeK Eq a => Eq (PacketD a)
deriving instance ShapeK Show a => Show (PacketD a)

sameInt :: BoxD Int -> BoxD Int -> Bool
sameInt = (==)
differentBool :: BoxD Bool -> BoxD Bool -> Bool
differentBool = (/=)
renderInt :: BoxD Int -> String
renderInt = show
precBool :: Int -> BoxD Bool -> ShowS
precBool = showsPrec
listInt :: [BoxD Int] -> ShowS
listInt = showList
packetInt :: PacketD Int -> PacketD Int -> Bool
packetInt = (==)
packetBool :: PacketD Bool -> String
packetBool = show
