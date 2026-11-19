module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot, failure)

type Views = M.PairT (G.PacketT Int) (G.PacketT Bool)

generic :: M.Th t -> M.Th Bool -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Bool -> Int -> M.MemoristM Views
call flag n = do
    unused <- M.lazy (error "unused argument")
    choice <- M.lazy (pure flag)
    count <- M.lazy (pure n)
    generic unused choice count

paid :: Bool -> Int -> M.MemoristM Views
paid flag n = do
    unused <- M.lazy (M.tick >> pure ())
    choice <- M.lazy (M.tick >> pure flag)
    count <- M.lazy (M.tick >> pure n)
    generic unused choice count

sourceFields :: Bool -> Int -> Int
sourceFields flag n = let (a,b) = Source.views () flag n in field a + field b
  where
    field (Source.Packet fixed _) = fixed

roots :: Views -> M.MemoristM Int
roots (M.PairT a b) = (+) <$> (tag <$> M.force a) <*> (tag <$> M.force b)
  where
    tag (G.PacketT _ _) = 1

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

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

probes :: [Probe]
probes =
    [ scalarSource "producer/fields" (\s -> sourceFields (number s > 0) (number s))
        (\s -> call (number s > 0) (number s) >>= fields)
    , scalarSource "producer/fully-demanded" (\s -> sourceFields (number s > 0) (number s))
        (\s -> call (number s > 0) (number s) >>= fully)
    , scalarSource "producer/roots" (const 2)
        (\s -> call (number s > 0) (number s) >>= roots)
    , scalarSource "producer/optional" (const 0)
        (\s -> call (number s > 0) (number s) >>= optionals)
    , scalarSource "producer/skip-paid-inputs" (const 0)
        (\s -> paid (number s > 0) (number s) >> pure 0)
    , scalarSource "producer/roots-skip-paid-count" (const 2)
        (\s -> paid (number s > 0) (number s) >>= roots)
    , scalarSource "producer/optional-skip-paid-count" (const 0)
        (\s -> paid (number s > 0) (number s) >>= optionals)
    , scalarSource "producer/paid-full" (\s -> sourceFields (number s > 0) (number s))
        (\s -> paid (number s > 0) (number s) >>= fully)
    , scalarSource "producer/repeated-demands" (\s -> repeats s * sourceFields True (number s)) $ \s -> do
        cached <- M.lazy (call True (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "producer/fresh-invocations" (\s -> repeats s * sourceFields False (number s)) $ \s -> do
        input <- M.lazy (pure ())
        flag <- M.lazy (pure False)
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic input flag count >>= fully)
    , scalarSource "producer/unused-bottom-count" (const 0)
        (const (call False (error "unused count") >>= optionals))
    , failure "producer/demanded-bottom-count"
        (const (call False (error "demanded count") >>= fields))
    , snapshot "producer/cross-type-root-and-field-aliases" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call (number s > 0) (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "producer/fresh-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure ())
        flag <- M.lazy (pure False)
        count <- M.lazy (pure (number s))
        (a,b) <- generic input flag count >>= names
        (c,d) <- generic input flag count >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    ]
