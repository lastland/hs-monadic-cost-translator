module Case (probes) where
import Generated hiding (BoxT(..))
import ReferenceSupport
probes :: [Probe]
probes = [scalar "hidden-import" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))]
