module Case (probes) where
import Generated (BoxT(MakeBox))
import ReferenceSupport
probes :: [Probe]
probes = [scalar "empty-record" (\s -> pure (case MakeBox {} of MakeBox _ -> number s))]
