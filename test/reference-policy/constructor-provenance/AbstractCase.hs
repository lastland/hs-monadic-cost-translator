module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "abstract-type" (\s -> pure (case Hidden (number s) of Hidden n -> n))]
