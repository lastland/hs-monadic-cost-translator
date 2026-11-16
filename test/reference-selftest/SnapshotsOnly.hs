module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
probes :: [Probe]
probes = [snapshot "no cost comparison" (\s -> M.value (M.eval1 (M.fromPure1 doubleM (number s))))]
