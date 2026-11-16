module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.folded (undefined :: Bool, number sample))
          (\sample -> M.fromPure1 foldedM (undefined :: Bool, number sample))
    , scalarSource "other input" (\sample -> Source.folded (undefined :: Bool, other sample))
          (\sample -> M.fromPure1 foldedM (undefined :: Bool, other sample))
    , scalarSource "defined unused field" (\sample -> Source.folded (leftDemand sample, number sample))
          (\sample -> M.fromPure1 foldedM (leftDemand sample, number sample))
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 foldedM (undefined :: Bool, number sample))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
