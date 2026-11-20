module Case (probes) where
import qualified Generated
import ReferenceSupport
data BoxT = MakeBox Int
probes :: [Probe]
probes = [scalar "local" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))]
