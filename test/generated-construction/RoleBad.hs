{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module RoleBad where
import qualified Memorist.Memorist as M
import RenamedUtility (Permit)
data Bad f a = Bad (f a)
foreignDemand :: Permit f a => Bad f a -> M.Dem (f a)

{-# LINE 50 "colliding-roles.hs" #-}
foreignDemand (Bad x) = M.fullDemand x
