module Generated where

import Memorist.Memorist
import qualified Memorist.Example.List as ML
import qualified Memorist.Example.NewtonRaphson as NR

stepM :: Num a => Th a -> MemoristM a
stepM tx = do
    tick
    x <- force tx
    pure (x + x)

repeatedM :: Num a => Th a -> MemoristM a
repeatedM tx = do
    tick
    -- The imported implementation deliberately has no iteration tick.
    stream <- lazy (NR.repeatlyM stepM tx)
    afterFirst <- lazy (ML.tailM stream)
    ML.headM afterFirst
