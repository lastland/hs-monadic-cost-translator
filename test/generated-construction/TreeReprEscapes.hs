{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module TreeReprEscapes where

import TreeRepr (ReprK, RepOf, TreeT)
import qualified TreeReprSource as Source
import qualified Memorist.Memorist as M

safe :: Source.Tree Int -> M.MemoristM (TreeT Int)
safe = M.reprM

open :: M.Repr a => a -> M.MemoristM (M.Rep a)
open = M.reprM

openTree :: ReprK a => Source.Tree a -> M.MemoristM (TreeT (RepOf a))
openTree = M.reprM

genericCarrier :: M.Memorist s m => Source.Tree Int -> m (TreeT Int)
genericCarrier = M.reprM

unusedObserver :: Int -> Int
unusedObserver = M.abstract

unusedRaw :: M.Th Int -> Int
unusedRaw = M.tval
