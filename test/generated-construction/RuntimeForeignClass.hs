module RuntimeForeignClass where

import Prelude hiding (Eq(..))
import qualified Memorist.Data.List as L

class Eq a where
    (==) :: a -> a -> Bool
instance Eq (L.ListD a) where
    _ == _ = True

same :: L.ListD Int -> L.ListD Int -> Bool
same = (==)
