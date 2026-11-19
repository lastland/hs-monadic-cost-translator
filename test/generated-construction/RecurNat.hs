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
