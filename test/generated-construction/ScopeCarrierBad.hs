{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, TypeFamilies #-}
module ScopeCarrierBad where
import ParamRepr (Box)
import qualified Memorist.Memorist as M

data Hidden = forall x. Hidden (Box x)
data Envelope a = Envelope a Hidden
instance M.Repr (Envelope a) where
    type Rep (Envelope a) = Envelope a
    reprM env@(Envelope _ (Hidden (x :: Box b))) =
        let unused = M.reprM x :: M.MemoristM (Box b) in pure env
    abstract = id
