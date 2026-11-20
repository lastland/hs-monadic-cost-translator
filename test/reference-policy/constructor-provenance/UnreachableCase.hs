module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "unreachable" (pure . number)]
unused s = MakeBox (number s)
