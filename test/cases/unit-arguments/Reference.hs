module Generated (ignoreM, applyM) where

import qualified Memorist.Memorist as M

-- ignore pays one entry tick and never demands its unit argument.
ignoreM :: M.Th () -> M.MemoristM Int
ignoreM _ = M.tick >> pure 7

-- apply pays its own entry tick plus ignore's; unit and addition are unticked.
applyM :: M.Th Int -> M.MemoristM Int
applyM argument = do
    M.tick
    unit <- M.lazy (pure ())
    ignored <- ignoreM unit
    value <- M.force argument
    pure (ignored + value)
