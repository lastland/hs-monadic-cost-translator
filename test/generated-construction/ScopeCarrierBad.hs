{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies #-}
module ScopeCarrierBad where
import FieldForward (Marked)
import qualified Memorist.Memorist as M

data Hidden = forall x. Eq x => Hidden (Marked x)
data Envelope a = Envelope a Hidden
instance M.Repr (Envelope a) where
    type Rep (Envelope a) = Envelope a
    reprM env@(Envelope _ (Hidden (x :: Marked b))) =
        let unused = M.reprM x :: M.MemoristM (Marked b) in pure env
    abstract = id
