{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module ContextReprBad where

import ContextRepr
import qualified Memorist.Memorist as M

given :: M.Repr Int => Box Int -> M.MemoristM (Box Int)
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
