{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module RoleGood where
import qualified Memorist.Memorist as M
import RenamedUtility (Permit)
data Good f a = Good (f a)
instance Permit f a => M.Demand (Good f a) where
    type Dem (Good f a) = M.Dem (f a)
{-# LINE 50 "colliding-roles.hs" #-}
    fullDemand (Good x) = M.fullDemand x
