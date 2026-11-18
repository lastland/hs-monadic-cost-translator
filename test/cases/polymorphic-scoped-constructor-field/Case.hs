module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot)

type Views = M.PairT (G.PacketT Int) (G.PacketT Bool)

generic :: M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Int -> M.MemoristM Views
call n = do
    input <- M.lazy (pure ())
    count <- M.lazy (M.tick >> pure n)
    generic input count

sourceFields :: Int -> Int
sourceFields n = let (a,b) = Source.views () n in field a + field b
  where
    field (Source.Packet fixed _) = fixed

fields :: Views -> M.MemoristM Int
fields (M.PairT a b) = (+) <$> (M.force a >>= field) <*> (M.force b >>= field)
  where
    field (G.PacketT fixed _) = M.force fixed

fully :: Views -> M.MemoristM Int
fully value@(M.PairT a b) = do
    result <- fields value
    left <- M.force a >>= optional
    right <- M.force b >>= optional
    pure (result + left + right)
  where
    optional (G.PacketT _ thunk) = do
        tag <- M.force thunk
        pure (case tag of M.NothingT -> 0; M.JustT _ -> 1)

names :: Views -> M.MemoristM ([M.ThName], [M.ThName])
names (M.PairT a b) = (,) <$> nodes a <*> nodes b
  where
    nodes root = do
        G.PacketT fixed optional <- M.force root
        pure [M.tname root, M.tname fixed, M.tname optional]

probes :: [Probe]
probes =
    [ scalarSource "scoped/ordinary-field" (sourceFields . number)
        (\s -> call (number s) >>= fields)
    , scalarSource "scoped/fully-demanded" (sourceFields . number)
        (\s -> call (number s) >>= fully)
    , scalarSource "scoped/skip-paid-input" (const 0)
        (\s -> call (number s) >> pure 0)
    , scalarSource "scoped/cached-demands" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "scoped/fresh-invocations" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        input <- M.lazy (pure ())
        count <- M.lazy (M.tick >> pure (number s))
        sum <$> replicateM (repeats s) (generic input count >>= fully)
    , snapshot "scoped/cross-type-identities" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "scoped/fresh-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure ())
        count <- M.lazy (pure (number s))
        (a,b) <- generic input count >>= names
        (c,d) <- generic input count >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    ]

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
