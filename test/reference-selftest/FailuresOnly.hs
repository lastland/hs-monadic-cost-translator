module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
probes :: [Probe]
probes = [failure "no successful cost comparison" (\_ -> M.fromPure1 doubleM (undefined :: Int))]
