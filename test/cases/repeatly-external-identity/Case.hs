module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.repeated (number sample))
          (\sample -> M.fromPure1 repeatedM (number sample))
    , scalarSource "other input" (\sample -> Source.repeated (other sample))
          (\sample -> M.fromPure1 repeatedM (other sample))
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 repeatedM (number sample))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
