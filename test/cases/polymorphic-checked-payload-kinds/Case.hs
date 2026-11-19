{-# LANGUAGE ExplicitForAll #-}
module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot, failure)

type Views = M.PairT (G.BoxT M.PairT) (G.BoxT M.PairT)

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
    field :: Source.Box f -> Int
    field (Source.Box fixed _) = fixed

sourceFirst :: Int -> Int
sourceFirst n = let (Source.Box fixed _, _) = Source.views () n in fixed

sourceRoots :: Int -> Int
sourceRoots n = let (a,b) = Source.views () n in root a + root b
  where
    root :: Source.Box f -> Int
    root (Source.Box _ _) = 1

sourceOptionals :: Int -> Int
sourceOptionals n = let (a,b) = Source.views () n in tag a + tag b
  where
    tag :: Source.Box f -> Int
    tag (Source.Box _ Nothing) = 0
    tag (Source.Box _ (Just _)) = 1

roots :: Views -> M.MemoristM Int
roots (M.PairT a b) = M.force a >> M.force b >> pure 2

field :: G.BoxT f -> M.MemoristM Int
field (G.BoxT fixed _) = M.force fixed

fields :: Views -> M.MemoristM Int
fields (M.PairT a b) = (+) <$> (M.force a >>= field) <*> (M.force b >>= field)

optionals :: Views -> M.MemoristM Int
optionals (M.PairT a b) = (+) <$> tag a <*> tag b
  where
    tag root = do
        G.BoxT _ optional <- M.force root
        value <- M.force optional
        pure (case value of M.NothingT -> 0; M.JustT _ -> 1)

fully :: Views -> M.MemoristM Int
fully value = (+) <$> fields value <*> optionals value

firstField :: Views -> M.MemoristM Int
firstField (M.PairT a _) = M.force a >>= field

names :: Views -> M.MemoristM ([M.ThName], [M.ThName])
names (M.PairT a b) = (,) <$> nodes a <*> nodes b
  where
    nodes root = do
        G.BoxT fixed optional <- M.force root
        pure [M.tname root, M.tname fixed, M.tname optional]

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

cost :: M.MemoristM Int -> Integer
cost program = let result = M.eval1 program
    in MT.toInteger (M.calcCost result (M.fullDemand (M.value result)))

probes :: [Probe]
probes =
    [ scalarSource "checked-binary/full" (sourceFields . number)
        (\s -> call False (number s) >>= fully)
    , scalarSource "checked-binary/min-bound" (const (sourceFields minBound))
        (const (call False minBound >>= fully))
    , scalarSource "checked-binary/max-bound" (const (sourceFields maxBound))
        (const (call False maxBound >>= fully))
    , scalarSource "checked-binary/negative" (const (sourceFields (-1)))
        (const (call False (-1) >>= fully))
    , scalarSource "checked-binary/zero" (const (sourceFields 0))
        (const (call False 0 >>= fully))
    , scalarSource "checked-binary/positive" (const (sourceFields 1))
        (const (call False 1 >>= fully))
    , scalarSource "checked-binary/roots" (sourceRoots . number)
        (\s -> call False (number s) >>= roots)
    , scalarSource "checked-binary/optional" (sourceOptionals . number)
        (\s -> call False (number s) >>= optionals)
    , scalarSource "checked-binary/first" (sourceFirst . number)
        (\s -> call False (number s) >>= firstField)
    , scalarSource "checked-binary/skip-paid" (const 0)
        (\s -> call True (number s) >> pure 0)
    , scalarSource "checked-binary/roots-skip-paid" (sourceRoots . number)
        (\s -> call True (number s) >>= roots)
    , scalarSource "checked-binary/optional-skip-paid" (sourceOptionals . number)
        (\s -> call True (number s) >>= optionals)
    , scalarSource "checked-binary/full-paid-once" (sourceFields . number)
        (\s -> call True (number s) >>= fully)
    , scalarSource "checked-binary/bottom-skip" (const 0)
        (const (call False (error "unused count") >> pure 0))
    , scalarSource "checked-binary/bottom-roots" (const (sourceRoots (error "unused source count")))
        (const (call False (error "unused count") >>= roots))
    , scalarSource "checked-binary/bottom-optional" (const (sourceOptionals (error "unused source count")))
        (const (call False (error "unused count") >>= optionals))
    , failure "checked-binary/demanded-bottom"
        (const (call False (error "demanded count") >>= firstField))
    , scalarSource "checked-binary/cached" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        cached <- M.lazy (call False (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "checked-binary/fresh" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        ignored <- M.lazy (error "unused argument")
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic ignored count >>= fully)
    , scalarSource "checked-binary/fresh-paid-input" (\s -> repeats s * sourceFields (number s)) $ \s -> do
        ignored <- M.lazy (error "unused argument")
        count <- M.lazy (M.tick >> pure (number s))
        sum <$> replicateM (repeats s) (generic ignored count >>= fully)
    , snapshot "checked-binary/aliases" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- call False (number s) >>= names
        pure (a == b && length a == 3 && distinct a)
    , snapshot "checked-binary/fresh-identities" $ \s -> M.value $ M.eval1 $ do
        ignored <- M.lazy (error "unused argument")
        count <- M.lazy (pure (number s))
        (a,b) <- generic ignored count >>= names
        (c,d) <- generic ignored count >>= names
        pure (a == b && c == d && distinct a && distinct c && all (`notElem` c) a)
    , snapshot "checked-binary/worked-schedule" $ \s ->
        [cost (call False (number s) >> pure 0)
        ,cost (call False (number s) >>= roots)
        ,cost (call True (number s) >>= optionals)
        ,cost (call False (number s) >>= firstField)
        ,cost (call False (number s) >>= fully)
        ,cost (call True (number s) >>= fully)
        ,cost (do cached <- M.lazy (call False (number s)); sum <$> replicateM 4 (M.force cached >>= fully))
        ,cost (sum <$> replicateM 4 (call False (number s) >>= fully))]
    ]
