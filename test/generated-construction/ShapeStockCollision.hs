{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module ShapeStockCollision where
import Data.Kind (Type)
import qualified Memorist.Memorist as M
import ShapeStock (ShapeK)
data Collision (f :: Type -> Type) a = Collision (M.ThD (f a))
{-# LINE 323 "Collision.hs" #-}
deriving instance (ShapeK Eq f, ShapeK Eq a) => Eq (Collision f a)
{-# LINE 323 "Collision.hs" #-}
collision :: (ShapeK Eq f, ShapeK Eq a) => Collision f a -> Collision f a -> Bool
collision = (==)
