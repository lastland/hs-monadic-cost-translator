{-# LANGUAGE TypeFamilies #-}
module HomeReprUnsafe where

import qualified Memorist.Memorist as M

data Box = Box (M.Th Int)

instance M.Repr Box where
    type Rep Box = Box
    reprM box = let unused = id <$> [True] in pure box
    abstract (Box thunk) = let unused = M.tval thunk in Box thunk

selected :: M.MemoristM Box
selected = M.reprM
    (let unavailable = undefined in Box unavailable)
