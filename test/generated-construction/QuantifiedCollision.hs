{-# LANGUAGE FlexibleContexts #-}
module QuantifiedCollision where
import RenamedProof
import qualified Memorist.Memorist as M
{-# LINE 49 "same-physical-roles.hs" #-}
open :: M.Repr a => a -> M.MemoristM (M.Rep a)
open = M.reprM
