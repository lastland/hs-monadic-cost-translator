{-# LANGUAGE FlexibleContexts #-}
module ContextReprUse where

import ContextRepr
import qualified Memorist.Memorist as M

plain :: Box Int -> M.MemoristM (Box Int)
plain = M.reprM

nested :: Nested (Nested (Box Int)) -> M.MemoristM (Nested (Nested (Box Int)))
nested = M.reprM

mixed :: Tagged (Nested Int) Bool -> M.MemoristM (Tagged (Nested Int) Bool)
mixed = M.reprM

inert :: Marked Bool -> M.MemoristM (Marked Bool)
inert = M.reprM

forward :: Forward Int -> M.MemoristM (Forward Int)
forward = M.reprM

opaqueForward :: Envelope Bool -> M.MemoristM (Envelope Bool)
opaqueForward = M.reprM

-- GHC selects the concrete child here despite the redundant constraint.
redundant :: M.Repr Int => Box Int -> M.MemoristM (Box Int)
redundant = M.reprM
