module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "private-child" (\s -> pure (case PrivateBox (number s) of PrivateBox n -> n))]
