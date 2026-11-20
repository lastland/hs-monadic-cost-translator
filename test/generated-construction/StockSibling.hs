{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, UndecidableInstances #-}
module StockSibling where

import qualified Memorist.Memorist as M

class Eq a => Selected a where
    sibling :: a -> a -> Bool
instance Eq a => Selected a where
    sibling x y = x == y

data ViaD a = ViaD (M.ThD a)
deriving instance Selected a => Eq (ViaD a)

same :: ViaD Int -> ViaD Int -> Bool
same = (==)
escape :: Int -> Int -> Bool
escape = sibling

deadHelper :: Eq a => a -> Bool
deadHelper x = let sibling _ = x == x in True
