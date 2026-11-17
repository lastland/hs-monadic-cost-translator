module Generated (sharedM, badM, fM, gM, testM) where

import qualified Memorist.Memorist as M

-- The global literal itself has no source-function entries. Only f, g, and
-- test contribute ticks, so one direct test costs three and two cost six.
sharedM, badM :: M.MemoristM Int
sharedM = pure 10
badM = pure (error "demanded global failure")

fM, gM, testM :: M.Th () -> M.MemoristM Int
fM _ = M.tick >> pure 10
gM _ = M.tick >> pure 10
testM _ = do
    M.tick
    left <- M.fromPure1 fM ()
    right <- M.fromPure1 gM ()
    pure (left + right)
