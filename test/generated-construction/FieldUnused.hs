{-# LANGUAGE TypeFamilies #-}
module FieldUnused where
import qualified Memorist.Memorist as M

data Box a = Box a
instance Eq a => M.Repr (Box a) where
    type Rep (Box a) = Box a
    reprM box@(Box x) = let unused = x == x in pure box
    abstract = id

selected :: Box Bool -> M.MemoristM (Box Bool)
selected = M.reprM
