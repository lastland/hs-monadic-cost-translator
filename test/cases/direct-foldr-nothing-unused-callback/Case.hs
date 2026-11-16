module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\_ -> Source.folded (Nothing :: Maybe Int))
          (\_ -> M.fromPure1 foldedM (Nothing :: Maybe Int))
    , scalarSource "nonempty invokes failing callback" (\sample -> Source.folded (Just (number sample)))
          (\sample -> M.fromPure1 foldedM (Just (number sample)))
    , scalar "repeated empty folds" $ \sample -> do
          values <- sequence (replicate (repeats sample)
              (M.fromPure1 foldedM (Nothing :: Maybe Int)))
          pure (sum values)
    ]
