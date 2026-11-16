module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "literal" (Source.literal . number) (\s -> M.fromPure1 literalM (number s))
    , failure "literal-zero" (\_ -> M.fromPure1 literalM (0 :: Int))
    , scalar "nonzero-fallback" (\s -> M.fromPure1 literalM (abs (other s) + 1))
    , scalarSource "direct-equations" (Source.choose . other) (\s -> M.fromPure1 chooseM (other s))
    , failure "bottom-pattern" (\_ -> M.fromPure1 literalM (undefined :: Int))
    ]
