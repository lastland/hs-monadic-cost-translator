{-# LANGUAGE ExplicitForAll #-}
module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot)

type Views = M.PairT (G.PacketT Int) (G.PacketT Bool)

-- Ordinary callers must not acquire a constraint on the unused argument.
generic :: forall t. M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Int -> M.MemoristM Views
call n = do
    input <- M.lazy (M.tick >> pure ())
    count <- M.lazy (pure n)
    generic input count

paid :: Int -> M.MemoristM Views
paid n = do
    input <- M.lazy (M.tick >> pure ())
    count <- M.lazy (M.tick >> pure n)
    generic input count

sourceRoots :: Int -> Int
sourceRoots n = let (a,b) = Source.views () n in root a + root b
  where
    root :: Source.Packet a -> Int
    root (Source.Packet _ _) = 1

sourceFields :: Int -> Int
sourceFields n = let (a,b) = Source.views () n in field a + field b
  where
    field :: Source.Packet a -> Int
    field (Source.Packet fixed _) = fixed

sourceOptional :: Int -> Int
sourceOptional n = let (a,b) = Source.views () n in tag a + tag b
  where
    tag :: Source.Packet a -> Int
    tag (Source.Packet _ Nothing) = 0
    tag (Source.Packet _ (Just _)) = 1

sourceFirst :: Int -> Int
sourceFirst n = let (Source.Packet fixed _, _) = Source.views () n in fixed

roots :: Views -> M.MemoristM Int
roots (M.PairT a b) = (+) <$> (root <$> M.force a) <*> (root <$> M.force b)
  where
    root :: G.PacketT a -> Int
    root (G.PacketT _ _) = 1

fields :: Views -> M.MemoristM Int
fields (M.PairT a b) = (+) <$> (M.force a >>= field) <*> (M.force b >>= field)
  where
    field :: G.PacketT a -> M.MemoristM Int
    field (G.PacketT fixed _) = M.force fixed

optionals :: Views -> M.MemoristM Int
optionals (M.PairT a b) = (+) <$> (M.force a >>= tag) <*> (M.force b >>= tag)
  where
    tag :: G.PacketT a -> M.MemoristM Int
    tag (G.PacketT _ optional) = do
        value <- M.force optional
        pure (case value of M.NothingT -> 0; M.JustT _ -> 1)

fully :: Views -> M.MemoristM Int
fully value = (+) <$> fields value <*> optionals value

firstField :: Views -> M.MemoristM Int
firstField (M.PairT a _) = do
    G.PacketT fixed _ <- M.force a
    M.force fixed

names :: Views -> M.MemoristM ([M.ThName], [M.ThName])
names (M.PairT a b) = (,) <$> nodes a <*> nodes b
  where
    nodes :: M.Th (G.PacketT a) -> M.MemoristM [M.ThName]
    nodes root = do
        G.PacketT fixed optional <- M.force root
        pure [M.tname root, M.tname fixed, M.tname optional]

probes :: [Probe]
probes =
    [ scalarSource "applied-packet/signed-fields" (sourceFields . number)
        (\s -> call (number s) >>= fields)
    , scalarSource "applied-packet/negative-boundary" (const (sourceFields (-1)))
        (const (call (-1) >>= fully))
    , scalarSource "applied-packet/zero" (const (sourceFields 0))
        (const (call 0 >>= fully))
    , scalarSource "applied-packet/positive-boundary" (const (sourceFields 1))
        (const (call 1 >>= fully))
    , scalarSource "applied-packet/min-bound-total-fields" (const (sourceFields minBound))
        (const (call minBound >>= fields))
    , scalarSource "applied-packet/max-bound-total-fields" (const (sourceFields maxBound))
        (const (call maxBound >>= fields))
    , scalarSource "applied-packet/roots-without-fields" (sourceRoots . number)
        (\s -> call (number s) >>= roots)
    , scalarSource "applied-packet/optional-without-int" (sourceOptional . number)
        (\s -> call (number s) >>= optionals)
    , scalarSource "applied-packet/first-fixed-only" (sourceFirst . number)
        (\s -> call (number s) >>= firstField)
    , scalarSource "applied-packet/fully-demanded" (sourceFields . number)
        (\s -> call (number s) >>= fully)
    , scalarSource "applied-packet/skip-all-paid-count" (const 0)
        (\s -> paid (number s) >> pure 0)
    , scalarSource "applied-packet/roots-skip-paid-count" (sourceRoots . number)
        (\s -> paid (number s) >>= roots)
    , scalarSource "applied-packet/optional-skip-paid-count" (sourceOptional . number)
        (\s -> paid (number s) >>= optionals)
    , scalarSource "applied-packet/full-demands-paid-count-once" (sourceFields . number)
        (\s -> paid (number s) >>= fully)
    , scalarSource "applied-packet/cached-repeated-demands" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "applied-packet/fresh-invocations" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        input <- M.lazy (M.tick >> pure ())
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic input count >>= fully)
    , snapshot "applied-packet/cross-type-root-int-and-maybe-aliases" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "applied-packet/fresh-root-int-and-maybe-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (M.tick >> pure ())
        count <- M.lazy (pure (number s))
        (a,b) <- generic input count >>= names
        (c,d) <- generic input count >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    ]

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
