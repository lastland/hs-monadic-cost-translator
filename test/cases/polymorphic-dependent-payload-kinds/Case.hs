{-# LANGUAGE DataKinds, ExplicitForAll, PolyKinds #-}
module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot, failure, cost, distinct)

type Views = M.PairT (G.PacketT M.MaybeT Int) (G.PacketT G.IndexT 'True)

generic :: forall t. M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Bool -> Int -> M.MemoristM Views
call paid n = do
    ignored <- M.lazy (error "unused argument")
    count <- M.lazy (if paid then M.tick >> pure n else pure n)
    generic ignored count

sourceFields :: Int -> Int
sourceFields n = let (a,b) = Source.views () n in field a + field b
  where
    field :: Source.Packet f a -> Int
    field (Source.Packet fixed _) = fixed

sourceFirst :: Int -> Int
sourceFirst n = let (Source.Packet fixed _, _) = Source.views () n in fixed

field :: G.PacketT f a -> M.MemoristM Int
field (G.PacketT fixed _) = M.force fixed

fields :: Views -> M.MemoristM Int
fields (M.PairT a b) = (+) <$> (M.force a >>= field) <*> (M.force b >>= field)

roots :: Views -> M.MemoristM Int
roots (M.PairT a b) = M.force a >> M.force b >> pure 2

optional :: M.Th (G.PacketT f a) -> M.MemoristM Int
optional root = do
    G.PacketT _ payload <- M.force root
    value <- M.force payload
    pure (case value of M.NothingT -> 0; M.JustT _ -> 1)

optionals :: Views -> M.MemoristM Int
optionals (M.PairT a b) = (+) <$> optional a <*> optional b

fully :: Views -> M.MemoristM Int
fully value = (+) <$> fields value <*> optionals value

first :: Views -> M.MemoristM Int
first (M.PairT a _) = M.force a >>= field

nodes :: M.Th (G.PacketT f a) -> M.MemoristM [M.ThName]
nodes root = do
    G.PacketT fixed payload <- M.force root
    pure [M.tname root, M.tname fixed, M.tname payload]

names :: Views -> M.MemoristM ([M.ThName], [M.ThName])
names (M.PairT a b) = (,) <$> nodes a <*> nodes b

probes :: [Probe]
probes =
    [ scalarSource "dependent-kinds/full" (sourceFields . number) (\s -> call False (number s) >>= fully)
    , scalarSource "dependent-kinds/min-bound" (const (sourceFields minBound)) (const (call False minBound >>= fully))
    , scalarSource "dependent-kinds/max-bound" (const (sourceFields maxBound)) (const (call False maxBound >>= fully))
    , scalarSource "dependent-kinds/roots" (const 2) (\s -> call True (number s) >>= roots)
    , scalarSource "dependent-kinds/optional" (const 0) (\s -> call True (number s) >>= optionals)
    , scalarSource "dependent-kinds/first" (sourceFirst . number) (\s -> call False (number s) >>= first)
    , scalarSource "dependent-kinds/skip" (const 0) (\s -> call True (number s) >> pure 0)
    , scalarSource "dependent-kinds/paid" (sourceFields . number) (\s -> call True (number s) >>= fully)
    , scalarSource "dependent-kinds/bottom-roots" (const 2) (const (call False (error "unused count") >>= roots))
    , scalarSource "dependent-kinds/bottom-optional" (const 0) (const (call False (error "unused count") >>= optionals))
    , failure "dependent-kinds/demanded-bottom" (const (call False (error "demanded count") >>= first))
    , scalarSource "dependent-kinds/cached" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        cached <- M.lazy (call False (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "dependent-kinds/fresh" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        ignored <- M.lazy (error "unused argument")
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic ignored count >>= fully)
    , snapshot "dependent-kinds/aliases-across-Type-and-Bool" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call False (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "dependent-kinds/fresh-identities" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call False (number s) >>= names
        (c,d) <- call False (number s) >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    , snapshot "dependent-kinds/worked-schedule" $ \s ->
        [cost (call False (number s) >> pure 0),cost (call False (number s) >>= roots)
        ,cost (call True (number s) >>= optionals),cost (call False (number s) >>= first)
        ,cost (call False (number s) >>= fully),cost (call True (number s) >>= fully)
        ,cost (do cached <- M.lazy (call False (number s)); sum <$> replicateM 4 (M.force cached >>= fully))
        ,cost (sum <$> replicateM 4 (call False (number s) >>= fully))]
    ]
