{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies,
    UndecidableInstances #-}
module ForeignSuper where
import qualified Memorist.Memorist as M

class M.Demand a => Foreign a where
    fullDemand :: a -> M.Dem a
    usage :: a -> M.Dem a -> M.USet
instance M.Demand a => Foreign a where
    fullDemand = M.fullDemand
    usage = M.usage

-- Same-spelled fields and ordinary superclass projections confer no role.
foreignDemand :: Foreign a => a -> M.Dem a
foreignDemand = M.fullDemand
foreignUsage :: Foreign a => a -> M.Dem a -> M.USet
foreignUsage = M.usage
foreignField :: Foreign a => a -> M.Dem a
foreignField = ForeignSuper.fullDemand

class M.Repr a => Counterfeit a where
    reprM :: a -> M.MemoristM (M.Rep a)
    abstract :: M.Rep a -> a
instance M.Repr a => Counterfeit a where
    reprM = M.reprM
    abstract = M.abstract
foreignRepr :: Counterfeit a => a -> M.MemoristM (M.Rep a)
foreignRepr = M.reprM
