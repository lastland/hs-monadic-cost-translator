{-# LANGUAGE TupleSections #-}
module State (attempt) where

import Control.Monad.State.Class (state)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM M.ThName
attempt = state (, M.nzero)
