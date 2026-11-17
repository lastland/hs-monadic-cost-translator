module Case (probes) where

import Generated (fM, gM, testM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

direct :: M.Th () -> M.MemoristM Int
direct = testM

first, second :: M.Th () -> M.MemoristM Int
first = fM
second = gM

probes :: [Probe]
probes =
    [ scalarSource "fixed-top-level-sharing" (const (Source.test ()))
        (const (M.fromPure1 direct ()))
    , scalarSource "uncached-top-level-calls" (const (Source.test () + Source.test ()))
        (const ((+) <$> M.fromPure1 direct () <*> M.fromPure1 direct ()))
    , scalarSource "cached-top-level-call" (const (Source.test () + Source.test ())) $ \_ -> do
        result <- M.lazy (M.fromPure1 direct ())
        (+) <$> M.force result <*> M.force result
    , scalarSource "independent-export-calls" (const (Source.f () + Source.g ())) $ \sample ->
        if even (variant sample)
        then (+) <$> M.fromPure1 first () <*> M.fromPure1 second ()
        else (+) <$> M.fromPure1 second () <*> M.fromPure1 first ()
    ]
