module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "type-only" (\s -> pure (const (number s) (undefined :: BoxT)))]
