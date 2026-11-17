module Case (probes) where

import Control.Monad (replicateM)
import Data.List (elemIndex)
import Generated (forwardM, plainM, workM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "work" (Source.work . number) (M.fromPure1 workM . number)
    , scalarSource "transitive-call" (Source.forward . other) (M.fromPure1 forwardM . other)
    , scalarSource "ordinary-interface" (uncurry (+) . Source.plain . number)
        (\sample -> M.fromPure1 plainM (number sample) >>= sumPair)
    , scalarSource "cached-fields" (\sample -> repeats sample * uncurry (+) (Source.plain (number sample))) $ \sample -> do
        pair <- M.fromPure1 plainM (number sample)
        sum <$> replicateM (repeats sample) (sumPair pair)
    , scalarSource "fresh-calls" (\sample -> repeats sample * uncurry (+) (Source.plain (other sample))) $ \sample ->
        sum <$> replicateM (repeats sample) (M.fromPure1 plainM (other sample) >>= sumPair)
    , demanded "unused-input" (const (const ())) (const (const (M.PairD M.Skip M.Skip)))
        (const (M.fromPure1 plainM (undefined :: Int)))
    , failure "demanded-input" (const (M.fromPure1 plainM (undefined :: Int) >>= sumPair))
    , snapshot "no-hidden-setup" resources
    ]

sumPair :: M.PairT Int Int -> M.MemoristM Int
sumPair (M.PairT left right) = (+) <$> M.force left <*> M.force right

resources :: Sample -> ([Bool], Maybe Int)
resources sample =
    let program = do
            input <- M.lazy (pure (number sample))
            M.PairT left right <- plainM input
            pure [M.tname left /= M.tname right, M.tname left /= M.tname input,
                  M.tname right /= M.tname input]
        (result, next) = M.runM program M.nzero
    in (M.value result, elemIndex next (take 16 (iterate M.nnext M.nzero)))
