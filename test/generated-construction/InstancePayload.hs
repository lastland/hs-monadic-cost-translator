{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module InstancePayload where

import qualified Memorist.Memorist as M

class Peek a where
    peek :: M.Th a -> a

instance Peek Int where
    peek = M.tval

leak :: M.Th Int -> M.MemoristM Int
leak = pure . peek
