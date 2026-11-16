module Case (probes) where

import Generated
import ReferenceSupport
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.folded (Just (number sample)))
          (\sample -> M.fromPure1 foldedM (Just (number sample)))
    , scalarSource "other input" (\sample -> Source.folded (Just (other sample)))
          (\sample -> M.fromPure1 foldedM (Just (other sample)))
    , scalarSource "empty container" (\_ -> Source.folded (Nothing :: Maybe Int))
          (\_ -> M.fromPure1 foldedM (Nothing :: Maybe Int))
    , scalar "shared result" $ \sample -> do
          result <- M.lazy (M.fromPure1 foldedM (Just (number sample)))
          values <- sequence (replicate (repeats sample) (M.force result))
          pure (sum values)
    ]
