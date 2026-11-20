module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as Target
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ demanded "paid-field" (\_ box -> case M.abstract box of Source.Box n -> n)
        (\_ (Target.BoxT field) -> Target.BoxD (M.fullDemand field)) paidBox
    , scalar "repeated-force" $ \s -> do
        Target.BoxT field <- paidBox s
        sum <$> replicateM (repeats s) (M.force field)
    ]

paidBox s = Target.BoxT <$> M.lazy (M.tick >> pure (number s))
