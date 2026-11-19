{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module NewtypeLabel where
import qualified Memorist.Memorist as M

class Eq a => Label a
instance Label Bool
instance Label [Int]

data Box a = Box a
instance Label a => M.Repr (Box a) where
    type Rep (Box a) = Bool
    reprM (Box x) = pure (x == x)
    abstract = undefined

selected :: Box Bool -> M.MemoristM Bool
selected = M.reprM
