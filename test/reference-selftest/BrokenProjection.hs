module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
probes :: [Probe]
probes = [demanded "broken shared projector" (\_ _ -> error "projection failed" :: Int)
    (\_ value -> value) (\s -> M.fromPure1 doubleM (number s))]
