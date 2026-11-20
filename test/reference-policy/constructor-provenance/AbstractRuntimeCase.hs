module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
probes :: [Probe]
probes = [scalar "private-constructor-not-conversion-type" (\s ->
    M.reprM (case Hidden (number s) of Hidden n -> n))]
