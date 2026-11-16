module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import ReferenceSupport

probes :: [Probe]
probes = concatMap check
    [ ("constant", Source.f, fM), ("plain", Source.g, gM)
    , ("parenthesized", Source.parenthesized, parenthesizedM)
    , ("annotated", Source.annotated, annotatedM), ("computed", Source.computed, computedM)
    ] ++ [failure "bad/demanded-error" (\sample -> M.fromPure1 badM (number sample))]
  where
    check (label, source, forward) =
        [ scalarSource (label ++ "/finite") (source . number) $ \sample -> do
            x <- M.lazy (replicateM_ (width sample + 1) M.tick >> pure (number sample))
            forward x
        , scalarSource (label ++ "/skipped-error")
            (\_ -> source (error "source argument should be unused")) $ \_ -> do
            x <- M.lazy (error "unused argument evaluated" :: M.MemoristM Int)
            forward x
        , scalar (label ++ "/repeated") $ \sample -> do
            x <- M.lazy (M.tick >> pure (other sample))
            sum <$> replicateM (repeats sample) (forward x)
        , snapshot (label ++ "/preparation-resources") (const (allocationRelation forward))
        ]

-- Compare allocator relations, never the particular names selected by either
-- implementation. The costed probes above remain the semantic comparison.
allocationRelation :: (M.Th Int -> M.MemoristM Int) -> (Bool, Bool, Integer)
allocationRelation forward =
    let program = do
            x <- M.lazy (error "allocation observation forced its argument")
            _ <- forward x
            pure (M.tname x)
        (result, next) = M.runM program M.nzero
        input = M.value result
    in (next == M.nnext input, next == M.nnext (M.nnext input),
        MT.toInteger (M.costAnnot result))
