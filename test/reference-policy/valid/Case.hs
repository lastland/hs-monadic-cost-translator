module Case (probes) where

import Generated
import ReferenceSupport
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalar "value" (\sample -> M.fromPure1 identityM (number sample))
    , demanded "demand" (const id) (const M.fullDemand)
        (\sample -> M.fromPure1 identityM (number sample))
    ] ++
    [ snapshot "sample" (\sample -> number sample)
    , failure "bottom" (const (M.fromPure1 identityM undefined))
    ]
