module Case (probes) where
import qualified Generated as Target
import ReferenceSupport
probes :: [Probe]
probes = [scalar "qualified-import" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))]
