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

-- The parent is a home instance, not an audited scalar instance. Only the
-- actual Eq Bool stored in its superclass field justifies this operation.
class Eq a => Label a where
    label :: a -> Bool
instance Label Bool where
    label = id

data Labelled a = Labelled a
instance Label a => M.Repr (Labelled a) where
    type Rep (Labelled a) = Bool
    reprM (Labelled x) = pure (x == x)
    abstract = undefined

homeParent :: Labelled Bool -> M.MemoristM Bool
homeParent = M.reprM
