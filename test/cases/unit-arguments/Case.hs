module Case (probes) where

import Generated (ignoreM, applyM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "unit-argument" (const (Source.ignore ()))
        (const (M.fromPure1 ignoreM ()))
    , scalarSource "constructed-unit-argument" (Source.apply . number)
        (M.fromPure1 applyM . number)
    , scalarSource "constructed-unit-argument-other" (Source.apply . other)
        (M.fromPure1 applyM . other)
    , scalarSource "unused-bottom-unit-argument" (const (Source.ignore undefined))
        (const (M.fromPure1 ignoreM (undefined :: ())))
    , failure "demanded-bottom-int-argument" (const (M.fromPure1 applyM (undefined :: Int)))
    ]
