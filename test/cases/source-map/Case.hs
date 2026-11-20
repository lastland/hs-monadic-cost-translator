module Case (probes) where

import Generated
import ReferenceSupport
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as ML
import qualified Memorist.Base.Thunk as MT
import qualified Example as Source

probes :: [Probe]
probes =
    [ scalar "keep" (\s -> M.fromPure1 keepM (number s))
    , demanded "map" (\_ -> M.abstract :: ML.ListT Int -> [Int]) (\_ -> M.fullDemand)
          (\s -> M.fromPure1 partialM [number s, other s])
    , demanded "caller/arbitrary-thunk" (\_ -> abstractBox) (\_ -> M.fullDemand)
          (\s -> pure (callerBox s))
    , snapshot "caller/arbitrary-accounting" $ \s ->
          let box@(BoxT field) = callerBox s
          in (abstractBox box, MT.toInteger (MT.acost (MT.tannot field)),
              MT.toInteger (M.sumcost (M.usage box (M.fullDemand box))))
    ]

callerBox :: Sample -> BoxT
callerBox s = BoxT (MT.Th (MT.fromInt 17) (number s) (MT.Annot (MT.Cost 1) MT.empty))

abstractBox :: BoxT -> Int
abstractBox box = case M.abstract box of Source.Box n -> n
