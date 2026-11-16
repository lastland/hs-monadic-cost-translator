module Case (probes) where
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalar "value" (\s -> M.fromPure1 doubleM (number s))
    , scalar "unused-bottom" (\_ -> M.fromPure1 ignoreM (undefined :: Int))
    , snapshot "alias-partition" (\s ->
        let program = do
                x <- M.lazy (pure (number s))
                M.PairT a b <- aliasesM x
                pure (M.tname x == M.tname a, M.tname a == M.tname b)
        in M.value (M.eval1 program))
    , snapshot "caller-allocation" (\s ->
        snd (M.runM (M.fromPure1 doubleM (number s)) M.nzero))
    ]
