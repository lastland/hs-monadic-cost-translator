module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.composed (number sample))
          (\sample -> M.fromPure1 composedM (number sample))
    , scalarSource "other input" (\sample -> Source.composed (other sample))
          (\sample -> M.fromPure1 composedM (other sample))
    , scalar "computed captured input" $ \sample -> M.fromPure1 (\tx -> do
          captured <- M.lazy (doubleM tx)
          composedM captured) (number sample)
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 composedM (number sample))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
