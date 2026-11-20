module Case (probes) where
import Generated (BoxT(MakeBox), NewBox(..), GBox(..), Standalone)
import ReferenceSupport
probes :: [Probe]
probes =
    [ scalar "explicit-child" (\s -> pure (case MakeBox (number s) of MakeBox n -> n))
    , scalar "newtype-wildcard" (\s -> pure (case NewBox (number s) of NewBox n -> n))
    , scalar "gadt-wildcard" (\s -> pure (case GBox (number s) of GBox n -> n))
    , scalar "standalone-constructor" (\s -> pure (case Standalone (number s) of Standalone n -> n))
    ]
