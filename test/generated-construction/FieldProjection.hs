{-# LANGUAGE TypeFamilies #-}
module FieldProjection where
import qualified Memorist.Memorist as M

data Project a = Project a
instance Ord a => M.Repr (Project a) where
    type Rep (Project a) = Bool
    reprM (Project x) = pure (x == x)
    abstract = undefined

selected :: Project Bool -> M.MemoristM Bool
selected = M.reprM
