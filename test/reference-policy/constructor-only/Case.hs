module Case (probes) where

import Generated (Token(..))
import ReferenceSupport

probes :: [Probe]
probes = [snapshot "constructor" (const Token), scalar "dummy" (pure . number)]
