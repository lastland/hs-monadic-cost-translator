module Case (probes) where
import Generated (NewBox)
import ReferenceSupport
probes :: [Probe]
probes = [scalar "bare-type-import" (\s -> pure (case NewBox (number s) of NewBox n -> n))]
