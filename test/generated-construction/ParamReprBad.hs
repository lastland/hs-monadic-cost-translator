{-# LANGUAGE ExistentialQuantification, FlexibleContexts, TypeFamilies #-}
module ParamReprBad where

import ParamRepr
import qualified Memorist.Memorist as M

givenCarrier :: M.Memorist s m => Box Int -> m (Box Int)
givenCarrier = M.reprM

openSelection :: Box a -> M.MemoristM (Box a)
openSelection = M.reprM

data Bad a = Bad a
instance M.Repr (Bad a) where
    type Rep (Bad a) = Bad a
    reprM box = let dead = id <$> [True] in pure box
    abstract box = let dead thunk = M.tval thunk in box

bad :: Bad Int -> M.MemoristM (Bad Int)
bad = M.reprM

data Context a = Context a
instance M.Repr a => M.Repr (Context a) where
    type Rep (Context a) = Context (M.Rep a)
    reprM (Context x) = Context <$> M.reprM x
    abstract = undefined

contextual :: Context Int -> M.MemoristM (Context Int)
contextual = M.reprM

data Hidden = forall b. Hidden (Box b)
data Holder a = Holder a Hidden
instance M.Repr (Holder a) where
    type Rep (Holder a) = Holder a
    reprM (Holder tag (Hidden hidden)) = (\box -> Holder tag (Hidden box)) <$> M.reprM hidden
    abstract = id

holder :: Holder Bool -> M.MemoristM (Holder Bool)
holder = M.reprM
