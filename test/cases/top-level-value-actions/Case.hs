module Case (probes) where

import Generated (badM, fM, gM, sharedM, testM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

direct :: M.Th () -> M.MemoristM Int
direct = testM

global :: M.MemoristM Int
global = sharedM

probes :: [Probe]
probes =
    [ scalarSource "global-action" (const Source.shared) (const global)
    , scalarSource "direct-call" (const (Source.test ())) (const (M.fromPure1 direct ()))
    , scalarSource "uncached-calls" (const (Source.test () + Source.test ()))
        (const ((+) <$> M.fromPure1 direct () <*> M.fromPure1 direct ()))
    , scalarSource "cached-call" (const (Source.test () + Source.test ())) $ \_ -> do
        result <- M.lazy (M.fromPure1 direct ())
        (+) <$> M.force result <*> M.force result
    , scalarSource "separate-exports" (const (Source.f () + Source.g ()))
        (const ((+) <$> M.fromPure1 fM () <*> M.fromPure1 gM ()))
    , scalarSource "unused-bottom-input" (const (Source.f (undefined :: ())))
        (const (M.fromPure1 fM (undefined :: ())))
    , scalarSource "demanded-global-failure" (const Source.bad) (const badM)
    ]
