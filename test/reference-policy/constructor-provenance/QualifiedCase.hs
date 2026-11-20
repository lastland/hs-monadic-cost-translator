module Case (probes) where
import qualified Generated as Target
import ReferenceSupport
probes :: [Probe]
probes = [scalar "wrong-qualifier" (\s -> pure (case Generated.MakeBox (number s) of Generated.MakeBox n -> n))]
