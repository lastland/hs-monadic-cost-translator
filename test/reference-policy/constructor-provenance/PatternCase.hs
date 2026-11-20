module Case (probes) where
import Generated
import ReferenceSupport
probes :: [Probe]
probes = [scalar "pattern-only" (\s -> pure (case undefined of MakeBox _ -> number s))]
