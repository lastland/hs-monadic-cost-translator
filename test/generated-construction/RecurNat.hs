{-# LANGUAGE TypeFamilies #-}
module RecurNat where
import qualified Memorist.Memorist as M

data Nat = Zero | Succ Nat
instance M.Repr Nat where
    type Rep Nat = Nat
    reprM Zero = pure Zero
    reprM (Succ n) = Succ <$> M.reprM n
    abstract = id

selected :: Nat -> M.MemoristM Nat
selected = M.reprM

data Mixed = Leaf | Node Mixed
instance M.Repr Mixed where
    type Rep Mixed = Mixed
    reprM Leaf = pure Leaf
    reprM (Node n) = let unused = M.reprM n :: M.MemoristM Mixed in Node <$> M.reprM n
    abstract = id

mixed :: Mixed -> M.MemoristM Mixed
mixed = M.reprM

data Chain a = End a | More (Chain a)
instance Eq a => M.Repr (Chain a) where
    type Rep (Chain a) = Chain a
    reprM (More xs) = More <$> M.reprM xs
    reprM end@(End x) = let unused = x == x in pure end
    abstract = id

safe :: Chain Bool -> M.MemoristM (Chain Bool)
safe = M.reprM
