module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot)

type Views = M.PairT (G.PacketT Int) (G.PacketT Bool)

-- This ordinary caller must not acquire a constraint on the unused argument.
generic :: M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Int -> M.MemoristM Views
call n = do
    input <- M.lazy (pure ())
    count <- M.lazy (pure n)
    generic input count

sourceRoots :: Int -> Int
sourceRoots n = let (a,b) = Source.views () n in root a + root b
  where
    root (Source.Packet _ _) = 1

sourceFields :: Int -> Int
sourceFields n = let (a,b) = Source.views () n in field a + field b
  where
    field (Source.Packet fixed _) = fixed

sourceOptional :: Int -> Int
sourceOptional n = let (a,b) = Source.views () n in tag a + tag b
  where
    tag (Source.Packet _ Nothing) = 0
    tag (Source.Packet _ (Just _)) = 1

roots :: Views -> M.MemoristM Int
roots (M.PairT a b) = (+) <$> (root <$> M.force a) <*> (root <$> M.force b)
  where
    root (G.PacketT _ _) = 1

fields :: Views -> M.MemoristM Int
fields (M.PairT a b) = (+) <$> (M.force a >>= field) <*> (M.force b >>= field)
  where
    field (G.PacketT fixed _) = M.force fixed

optionals :: Views -> M.MemoristM Int
optionals (M.PairT a b) = (+) <$> (M.force a >>= tag) <*> (M.force b >>= tag)
  where
    tag (G.PacketT _ optional) = do
        value <- M.force optional
        pure (case value of M.NothingT -> 0; M.JustT _ -> 1)

fully :: Views -> M.MemoristM Int
fully value = (+) <$> fields value <*> optionals value

names :: Views -> M.MemoristM ([M.ThName], [M.ThName])
names (M.PairT a b) = (,) <$> nodes a <*> nodes b
  where
    nodes root = do
        G.PacketT fixed optional <- M.force root
        pure [M.tname root, M.tname fixed, M.tname optional]

paid :: Int -> M.MemoristM Views
paid n = do
    input <- M.lazy (pure ())
    count <- M.lazy (M.tick >> pure n)
    generic input count

probes :: [Probe]
probes =
    [ scalarSource "packet/signed-fields" (sourceFields . number)
        (\s -> call (number s) >>= fields)
    , scalarSource "packet/min-bound-total-fields" (const (sourceFields minBound))
        (const (call minBound >>= fields))
    , scalarSource "packet/roots-without-fields" (sourceRoots . number)
        (\s -> call (number s) >>= roots)
    , scalarSource "packet/optional-without-int" (sourceOptional . number)
        (\s -> call (number s) >>= optionals)
    , scalarSource "packet/fully-demanded" (sourceFields . number)
        (\s -> call (number s) >>= fully)
    , scalarSource "packet/skip-all-paid-count" (const 0)
        (\s -> paid (number s) >> pure 0)
    , scalarSource "packet/roots-skip-paid-count" (sourceRoots . number)
        (\s -> paid (number s) >>= roots)
    , scalarSource "packet/optional-skip-paid-count" (sourceOptional . number)
        (\s -> paid (number s) >>= optionals)
    , scalarSource "packet/int-demands-paid-count-once" (sourceFields . number)
        (\s -> paid (number s) >>= fields)
    , scalarSource "packet/cached-repeated-demands" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "packet/fresh-invocations" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        input <- M.lazy (pure ())
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic input count >>= fully)
    , snapshot "packet/cross-type-root-int-and-maybe-aliases" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "packet/fresh-root-int-and-maybe-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure ())
        count <- M.lazy (pure (number s))
        (a,b) <- generic input count >>= names
        (c,d) <- generic input count >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    ]

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
