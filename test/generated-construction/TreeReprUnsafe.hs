{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module TreeReprUnsafe where

import TreeRepr (ReprK, RepOf)
import qualified Memorist.Memorist as M

data Before a = BeforeEnd | Before a (Before a)
data BeforeT a = BeforeEndT | BeforeT (M.Th a) (M.Th (BeforeT a))

instance forall a. ReprK a => M.Repr (Before a) where
    type Rep (Before a) = BeforeT (RepOf a)
    reprM BeforeEnd = pure BeforeEndT
    reprM (Before value rest) = do
        (M.abstract (0 :: Int) :: Int) `seq` pure ()
        child <- M.lazy (M.reprM rest)
        field <- M.lazy (M.reprM value)
        pure (BeforeT field child)
    abstract BeforeEndT = BeforeEnd
    abstract (BeforeT value rest) = Before (M.abstract (M.tval value)) (M.abstract (M.tval rest))

beforeCaller :: Before Int -> M.MemoristM (BeforeT Int)
beforeCaller = M.reprM

data After a = AfterEnd | After a (After a)
data AfterT a = AfterEndT | AfterT (M.Th a) (M.Th (AfterT a))

instance forall a. ReprK a => M.Repr (After a) where
    type Rep (After a) = AfterT (RepOf a)
    reprM AfterEnd = pure AfterEndT
    reprM (After value rest) = do
        child <- M.lazy (M.reprM rest)
        (M.abstract (0 :: Int) :: Int) `seq` pure ()
        field <- M.lazy (M.reprM value)
        pure (AfterT field child)
    abstract AfterEndT = AfterEnd
    abstract (AfterT value rest) = After (M.abstract (M.tval value)) (M.abstract (M.tval rest))

afterCaller :: After Bool -> M.MemoristM (AfterT Bool)
afterCaller = M.reprM
