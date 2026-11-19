{-# LANGUAGE TypeFamilies #-}
module FieldGrowth where
import qualified Memorist.Memorist as M

data Grow a = Grow a
instance M.Repr (Grow a) where
    type Rep (Grow a) = Grow a
    reprM box@(Grow x) = (\_ -> box) <$> M.reprM (Grow [x])
    abstract = id

selected :: Grow Bool -> M.MemoristM (Grow Bool)
selected = M.reprM
