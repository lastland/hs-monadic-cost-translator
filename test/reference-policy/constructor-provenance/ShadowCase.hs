module Case (probes) where
import Generated
import ReferenceSupport
data Local = MakeBox Int
probes :: [Probe]
probes = [scalar "shadow" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))]
