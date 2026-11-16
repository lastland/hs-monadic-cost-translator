module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "arithmetic" (Source.arithmetic . number) (\s -> M.fromPure1 arithmeticM (number s))
    , scalar "other-input" (\s -> M.fromPure1 arithmeticM (other s))
    , scalar "negative-odd-floor" (\s -> M.fromPure1 arithmeticM (negate (2 * abs (number s) + 1)))
    , scalar "zero" (\_ -> M.fromPure1 arithmeticM (0 :: Int))
    , failure "demanded-bottom" (\_ -> M.fromPure1 arithmeticM (undefined :: Int))
    ]
