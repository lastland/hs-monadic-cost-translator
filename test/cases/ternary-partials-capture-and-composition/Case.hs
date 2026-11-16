module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.partial (number sample))
          (\sample -> M.fromPure1 partialM (number sample))
    , scalarSource "other input" (\sample -> Source.partial (other sample))
          (\sample -> M.fromPure1 partialM (other sample))
    , scalar "computed captured input" $ \sample -> M.fromPure1 (\tx -> do
          captured <- M.lazy (twiceM tx)
          partialM captured) (number sample)
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 partialM (number sample))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
