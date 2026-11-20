module Case (probes) where
import Generated (Token(..))
import ReferenceSupport
probes :: [Probe]
probes = [scalar "implicit-export" (\s -> pure (case Token of Token -> number s))]
