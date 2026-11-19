{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module ContextReprCycle where
import qualified Memorist.Memorist as M

data Loop a = Loop a
instance M.Repr (Loop a) => M.Repr (Loop a) where
    type Rep (Loop a) = Loop a
    reprM = pure
    abstract = id

cycleUse :: Loop Int -> M.MemoristM (Loop Int)
cycleUse = M.reprM
