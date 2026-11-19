{-# LANGUAGE TypeFamilies #-}
module FieldForward where
import qualified Memorist.Memorist as M

data Marked a = Marked a
instance Eq a => M.Repr (Marked a) where
    type Rep (Marked a) = Marked a
    reprM = pure
    abstract = id

data Envelope a = Envelope (Marked a)
instance Eq a => M.Repr (Envelope a) where
    type Rep (Envelope a) = Envelope a
    reprM (Envelope x) = Envelope <$> M.reprM x
    abstract = id

selected :: Envelope [Int] -> M.MemoristM (Envelope [Int])
selected = M.reprM
