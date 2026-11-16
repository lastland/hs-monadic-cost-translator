module Case (probes) where

import Generated (identityM)
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes = [failure "not-a-successful-comparison" (const (M.fromPure1 identityM undefined))]
