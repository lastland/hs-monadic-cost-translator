module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "private-record" (\s -> pure (case PrivateBox {} of PrivateBox _ -> number s))]
