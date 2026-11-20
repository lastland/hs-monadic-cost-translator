module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
probes :: [Probe]
probes = [scalar "runtime-only" (\s -> M.lazy (M.tick >> pure (number s)) >>= M.force)]
