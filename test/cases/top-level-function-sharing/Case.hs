module Case (probes) where

import Generated (testM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

direct :: M.Th () -> M.MemoristM Int
direct = testM

probes :: [Probe]
probes =
    [ scalarSource "fixed-top-level-sharing" (const (Source.test ()))
        (const (M.fromPure1 direct ()))
    , scalarSource "uncached-top-level-calls" (const (Source.test () + Source.test ()))
        (const ((+) <$> M.fromPure1 direct () <*> M.fromPure1 direct ()))
    ]
