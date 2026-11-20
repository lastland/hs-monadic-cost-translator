module Case (probes) where
import Generated
import qualified Example as Source
import ReferenceSupport
probes :: [Probe]
probes = [scalar "unrelated-module" (\s -> pure (case Source.MakeBox (number s) of Source.MakeBox n -> n))]
