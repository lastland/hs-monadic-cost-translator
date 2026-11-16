module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.observed (number sample))
          (\sample -> M.fromPure1 observedM (number sample))
    , scalarSource "other input" (\sample -> Source.observed (other sample))
          (\sample -> M.fromPure1 observedM (other sample))
    , scalar "computed captured input" $ \sample -> M.fromPure1 (\tx -> do
          captured <- M.lazy (addM tx tx)
          observedM captured) (number sample)
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 observedM (number sample))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
