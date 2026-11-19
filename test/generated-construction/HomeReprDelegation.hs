{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module HomeReprDelegation where

import qualified Memorist.Memorist as M

data Box = Box

inject :: M.Memorist s m => Box -> m Box
inject = pure

instance M.Repr Box where
    type Rep Box = Box
    reprM = inject
    abstract = id

selected :: M.MemoristM Box
selected = M.reprM Box
