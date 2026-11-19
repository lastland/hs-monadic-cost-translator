{-# LANGUAGE BlockArguments, TypeFamilies #-}
module Construction where

import qualified Memorist.Memorist as M

ordinary :: M.MemoristM Int
ordinary = do
    thunk <- M.lazy do
        M.tick
        pure 3
    M.force thunk

data Box = Box Int
data BoxT = BoxT (M.Th Int)

-- Payload observation is admitted only as this verified Repr.abstract role,
-- never because an arbitrary helper happens to have a pure result type.
instance M.Repr Box where
    type Rep Box = BoxT
    reprM (Box number) = BoxT <$> M.lazy (M.reprM number)
    abstract (BoxT number) = Box (M.abstract (M.tval number))

-- M.value (M.eval1 action) is only a comment, not a reference.
description :: String
description = "Memorist.Base.Core.eval M.eval1 M.value runM runMMBT runMBT runAnnotT runMT"
