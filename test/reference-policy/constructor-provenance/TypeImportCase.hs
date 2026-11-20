module Case (probes) where
import Generated (BoxT)
import ReferenceSupport
probes :: [Probe]
probes = [scalar "type-import" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))]
