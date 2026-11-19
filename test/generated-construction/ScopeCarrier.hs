{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module ScopeCarrier where
import FieldForward (Marked)
import qualified Memorist.Memorist as M

data Envelope a = Envelope (Marked a)
instance Eq a => M.Repr (Envelope a) where
    type Rep (Envelope a) = Envelope a
    reprM env@(Envelope x) = let unused = M.reprM x :: M.MemoristM (Marked a) in pure env
    abstract = id

selected :: Envelope Bool -> M.MemoristM (Envelope Bool)
selected = M.reprM
