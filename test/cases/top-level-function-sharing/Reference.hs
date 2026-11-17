module Generated (fM, gM, testM) where

import qualified Memorist.Memorist as M

-- This reasoning model gives the single source binding its own identity,
-- outside the observer's ordinary nonnegative allocation roots. Its one
-- retained thunk contributes work's tick once; each call contributes the
-- three entry ticks of test, f, and g. The fully demanded total is four.
-- A second uncached test call pays those three entry ticks again, giving seven.
-- An explicitly cached test call retains all three entries, so two forces
-- still cost four including the shared work.
-- Independent f/g calls pay their two entries and the shared work, giving three.
shared :: M.Th Int
shared = M.value (M.eval (M.lazy (M.fromPure1 workM (5 :: Int))) (M.fromInt (-1)))

workM :: M.Th Int -> M.MemoristM Int
workM argument = do
    M.tick
    left <- M.force argument
    right <- M.force argument
    pure (left + right)

fM, gM, testM :: M.Th () -> M.MemoristM Int
fM _ = M.tick >> M.force shared
gM _ = M.tick >> M.force shared
testM _ = do
    M.tick
    left <- M.fromPure1 fM ()
    right <- M.fromPure1 gM ()
    pure (left + right)
