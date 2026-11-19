{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableSuperClasses #-}
module LabelFamilyList where
import qualified Memorist.Memorist as M

type family Payload a where
    Payload Bool = [Int]
class Eq (Payload a) => Label a
instance Label Bool

data Box a = Box (Payload a)
instance Label a => M.Repr (Box a) where
    type Rep (Box a) = Bool
    reprM (Box x) = pure (x == x)
    abstract = undefined

selected :: Box Bool -> M.MemoristM Bool
selected = M.reprM
