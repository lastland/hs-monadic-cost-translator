{-# LANGUAGE KindSignatures, StandaloneDeriving, UndecidableInstances #-}
module StockContext where
import Data.Kind (Type)

data PhantomD (a :: Type) = PhantomD Int
deriving instance Eq a => Eq (PhantomD a)

concrete :: PhantomD Int -> PhantomD Int -> Bool
concrete = (==)
ordinaryGiven :: Eq a => PhantomD a -> PhantomD a -> Bool
ordinaryGiven = (==)
