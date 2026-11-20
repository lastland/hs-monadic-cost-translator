{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module StockGrowth where
import qualified Memorist.Memorist as M

data GrowthD a = GrowthD (M.ThD a) (M.ThD (GrowthD (M.ThD a)))
deriving instance Eq a => Eq (GrowthD a)
deriving instance Show a => Show (GrowthD a)
same :: GrowthD Int -> GrowthD Int -> Bool
same = (==)
render :: GrowthD Bool -> String
render = show
