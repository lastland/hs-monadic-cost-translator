module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "type-not-constructor" (\s -> pure (case Ambiguous (number s) of Ambiguous n -> n))]
