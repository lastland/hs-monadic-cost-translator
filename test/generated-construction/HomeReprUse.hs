{-# LANGUAGE TypeFamilies #-}
module HomeReprUse where

import HomeReprControl (Box(..))
import qualified Memorist.Memorist as M

selected :: M.MemoristM (M.MaybeT Box)
selected = M.reprM (Just (Box 1))

data Outer = Outer Box

instance M.Repr Outer where
    type Rep Outer = Outer
    reprM (Outer box) = Outer <$> M.reprM box
    abstract = id

nested :: M.MemoristM Outer
nested = M.reprM (Outer (Box 1))
