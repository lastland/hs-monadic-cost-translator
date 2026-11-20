module CallerArithmeticInstance where

import qualified Memorist.Memorist as M

data Observed = Observed (M.Th Int)

instance Num Observed where
    Observed x + y = M.tval x `seq` y
    _ * y = y
    negate = id
    abs = id
    signum = id
    fromInteger _ = error "not used"
