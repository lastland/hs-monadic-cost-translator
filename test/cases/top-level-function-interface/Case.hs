module Case (probes) where

import Control.Monad (replicateM)
import Generated (plainM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

-- This type is the ordinary translated interface, with no module environment.
plain :: M.Th Int -> M.MemoristM (M.PairT Int Int)
plain = plainM

probes :: [Probe]
probes =
    [ scalarSource "direct-call" (uncurry (+) . Source.plain . number)
        (\sample -> M.fromPure1 plain (number sample) >>= sumPair)
    , scalarSource "repeated-demand" (\sample -> repeats sample * uncurry (+) (Source.plain (number sample))) $ \sample -> do
        pair <- M.fromPure1 plain (number sample)
        sum <$> replicateM (repeats sample) (sumPair pair)
    , demanded "unused-input" (const (const ())) (const (const (M.PairD M.Skip M.Skip)))
        (\_ -> M.fromPure1 plain (error "unused argument" :: Int))
    , failure "demanded-input" $ \_ -> do
        pair <- M.fromPure1 plain (error "demanded argument" :: Int)
        sumPair pair
    ]

sumPair :: M.PairT Int Int -> M.MemoristM Int
sumPair (M.PairT first second) = (+) <$> M.force first <*> M.force second
