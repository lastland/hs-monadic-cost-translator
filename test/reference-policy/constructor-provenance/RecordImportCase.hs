module Case (probes) where
import qualified Generated as G
import ReferenceSupport
probes :: [Probe]
probes = [scalar "wrong-record-import" (\s -> pure (case MakeBox {} of MakeBox _ -> number s))]
