module Case (probes) where

import Generated (fM)
import qualified Example as S
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "identityM" (S.f . number) (\s -> M.fromPure1 fM (number s))
    , scalarSource "identity-other" (S.f . other) (\s -> M.fromPure1 fM (other s))
    ]
