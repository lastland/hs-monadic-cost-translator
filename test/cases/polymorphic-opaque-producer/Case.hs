module Case (probes) where

import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Probe, Sample(..), scalarSource)

-- Negative occurrence: neither the caller nor the generated API has a Functor.
newtype Consumer a = Consumer (a -> Bool)

ordinary :: M.Th Int -> M.MemoristM (G.HigherT f a)
ordinary = G.makeM

views :: M.Th Int -> M.MemoristM (M.PairT (G.HigherT f Int) (G.HigherT f Bool))
views = G.viewsM

fixed :: G.HigherT f a -> M.MemoristM Int
fixed (G.HigherT n _) = M.force n

sourceFixed :: Source.Higher f a -> Int
sourceFixed (Source.Higher n _) = n

probes :: [Probe]
probes =
    [ scalarSource "ordinary" (\s -> sourceFixed (Source.make (number s))) $ \s -> do
        input <- M.lazy (M.tick >> pure (number s))
        x <- ordinary input :: M.MemoristM (G.HigherT Consumer Bool)
        fixed x
    , scalarSource "both-views" (\s -> let (x,y) = Source.views (number s)
                                      in sourceFixed x + sourceFixed y) $ \s -> do
        input <- M.lazy (M.tick >> pure (number s))
        M.PairT left right <- views input
        (+) <$> (M.force left >>= fixed) <*> (M.force right >>= fixed)
    ]
