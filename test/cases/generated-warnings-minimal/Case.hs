module Case (probes) where

import Generated (identityM)
import qualified Minimal as S
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "identityM" (S.identity . number) (\s -> M.fromPure1 identityM (number s))
    , scalarSource "identity-other" (S.identity . other) (\s -> M.fromPure1 identityM (other s))
    ]
