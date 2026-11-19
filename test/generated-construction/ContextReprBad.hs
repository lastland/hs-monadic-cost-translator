{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module ContextReprBad where

import ContextRepr
import qualified Memorist.Memorist as M

given :: M.Repr Missing => Box Missing -> M.MemoristM (Box Missing)
given = M.reprM

wrong :: M.Memorist s m => Nested Int -> m (Nested Int)
wrong = M.reprM

nonScalar :: Tagged Int [Int] -> M.MemoristM (Tagged Int Bool)
nonScalar = M.reprM

data Unsafe a = Unsafe a
instance M.Repr a => M.Repr (Unsafe a) where
    type Rep (Unsafe a) = Unsafe a
    reprM x = let unused = id <$> [True] in pure x
    abstract x = let unused th = M.tval th in x

data Missing = Missing

inertGiven :: Marker Missing => Marked Missing -> M.MemoristM (Marked Missing)
inertGiven = M.reprM

data Sibling a = Sibling a
instance Eq a => M.Repr (Sibling a) where
    type Rep (Sibling a) = Sibling a
    reprM x@(Sibling a) = pure (if a == a then x else x)
    abstract x@(Sibling a) = if a == a then x else x

class Inspect a where
    inspect :: a -> Bool
instance Inspect Bool where
    inspect = id
data InspectBox a = InspectBox a
instance Inspect a => M.Repr (InspectBox a) where
    type Rep (InspectBox a) = Bool
    reprM (InspectBox a) = pure (inspect a)
    abstract = undefined

unsafeUse :: InspectBox Bool -> M.MemoristM Bool
unsafeUse = M.reprM
