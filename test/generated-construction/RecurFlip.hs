{-# LANGUAGE TypeFamilies #-}
module RecurFlip where
import qualified Memorist.Memorist as M

data Flip a b = End | Step (Flip b a)
instance M.Repr (Flip a b) where
    type Rep (Flip a b) = Flip a b
    reprM End = pure End
    reprM (Step x) = Step <$> M.reprM x
    abstract = id

selected :: Flip Bool Int -> M.MemoristM (Flip Bool Int)
selected = M.reprM

data Checked a b = Stop a | Swap (Checked b a)
instance (Eq a, Eq b) => M.Repr (Checked a b) where
    type Rep (Checked a b) = Checked a b
    reprM (Swap x) = Swap <$> M.reprM x
    reprM end@(Stop x) = let unused = x == x in pure end
    abstract = id

checked :: Checked Bool Int -> M.MemoristM (Checked Bool Int)
checked = M.reprM
