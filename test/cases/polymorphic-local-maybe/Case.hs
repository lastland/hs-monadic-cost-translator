module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type Views = M.PairT (M.MaybeT Int) (M.MaybeT Bool)
type Nested = M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Bool))

dead :: Int -> Int
dead = error "outer input must remain unused"

probes :: [Probe]
probes =
    [ demanded "maybe/nothing-fully-demanded" (const pureRoots) (const M.fullDemand)
        (const (call 0))
    , scalarSource "maybe/just-bottom-shells" (sourceRoots . positive)
        (\s -> call (positive s) >>= countRoots)
    , demanded "maybe/selected-roots" projectSelected demandSelected (call . positive)
    , demanded "maybe/bottom-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (call (undefined :: Int)))
    , failure "maybe/demanded-negative-root" (\_ -> call (-1) >>= countRoots)
    , failure "maybe/demanded-bottom-count" (\_ -> call (undefined :: Int) >>= countRoots)
    , failure "maybe/demanded-int-payload" (\_ -> call 1 >>= intPayload)
    , failure "maybe/demanded-bool-payload" (\_ -> call 1 >>= boolPayload)
    , failure "control/outer-input-really-bottom" $ \_ -> do
        input <- M.lazy (pure dead)
        function <- M.force input
        pure (function 0)
    , scalarSource "maybe/fresh-invocations" (\s -> repeats s * sourceRoots (positive s)) $ \s -> do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        sum <$> replicateM (repeats s) (generic input count >>= countRoots)
    , scalarSource "maybe/cached-call-repeated-demands" (\s -> repeats s * sourceRoots (positive s)) $ \s -> do
        cached <- M.lazy (call (positive s))
        sum <$> replicateM (repeats s) (M.force cached >>= countRoots)
    , snapshot "maybe/cross-type-root-and-field-identities" $ \s -> M.value $ M.eval1 $ do
        names <- call (positive s) >>= viewNames
        pure (uncurry (==) names && length (fst names) == 2)
    , snapshot "maybe/fresh-root-and-field-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        first <- generic input count >>= viewNames
        second <- generic input count >>= viewNames
        pure (sharedFresh first second)
    , scalarSource "distinct/equal-rhs-stay-separate" (sourcePair . Source.distinct dead . positive) $ \s -> do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        genericDistinct input count >>= countRoots
    , snapshot "distinct/root-and-field-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        (first,second) <- genericDistinct input count >>= viewNames
        pure (length first == 2 && length second == 2 && disjoint first second)
    , demanded "nested/outer-nothing-fully-demanded" (const pureNested) (const M.fullDemand)
        (const (callNested (-1)))
    , demanded "nested/inner-nothing-fully-demanded" (const pureNested) (const M.fullDemand)
        (const (callNested 0))
    , demanded "nested/outer-roots-only" (const pureRoots) (const outerDemand)
        (callNested . positive)
    , demanded "nested/inner-roots-leaves-skipped" (const pureNested) (const innerDemand)
        (callNested . positive)
    , failure "nested/demanded-int-leaf" (\_ -> callNested 1 >>= intNestedLeaf)
    , failure "nested/demanded-bool-leaf" (\_ -> callNested 1 >>= boolNestedLeaf)
    , snapshot "nested/cross-type-root-inner-and-leaf-identities" $ \s -> M.value $ M.eval1 $ do
        names <- callNested (positive s) >>= nestedNames
        pure (uncurry (==) names && length (fst names) == 3)
    , snapshot "nested/fresh-root-inner-and-leaf-identities" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        first <- genericNested input count >>= nestedNames
        second <- genericNested input count >>= nestedNames
        pure (sharedFresh first second)
    , scalarSource "nested/fresh-invocations" (\s -> repeats s * sourceNested (positive s)) $ \s -> do
        input <- M.lazy (pure dead)
        count <- M.lazy (pure (positive s))
        sum <$> replicateM (repeats s) (genericNested input count >>= countNested)
    , scalarSource "nested/cached-call-repeated-demands" (\s -> repeats s * sourceNested (positive s)) $ \s -> do
        cached <- M.lazy (callNested (positive s))
        sum <$> replicateM (repeats s) (M.force cached >>= countNested)
    , scalarSource "nested/paid-captured-count-shared-across-fresh-calls" (\s -> repeats s * sourceNested (positive s)) $ \s -> do
        input <- M.lazy (pure dead)
        count <- M.lazy (M.tick >> pure (positive s))
        sum <$> replicateM (repeats s) (genericNested input count >>= countNested)
    ]

generic :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Bool))
generic = G.viewsM

genericNested :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Bool)))
genericNested = G.nestedM

genericDistinct :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Bool))
genericDistinct = G.distinctM

positive :: Sample -> Int
positive = (1 +) . abs . number

call :: Int -> M.MemoristM Views
call count = do
    input <- M.lazy (pure dead)
    n <- M.lazy (pure count)
    generic input n

callNested :: Int -> M.MemoristM Nested
callNested count = do
    input <- M.lazy (pure dead)
    n <- M.lazy (pure count)
    genericNested input n

sourceTag :: Maybe a -> Int
sourceTag Nothing = 0
sourceTag (Just _) = 1

sourcePair :: (Maybe a,Maybe b) -> Int
sourcePair (a,b) = sourceTag a + sourceTag b

sourceRoots :: Int -> Int
sourceRoots n = sourcePair (Source.views dead n)

sourceNested :: Int -> Int
sourceNested n = let (a,b) = Source.nested dead n in levels a + levels b
  where
    levels Nothing = 0
    levels (Just x) = 1 + sourceTag x

tag :: M.MaybeT a -> Int
tag M.NothingT = 0
tag (M.JustT _) = 1

pureRoots :: M.PairT (M.MaybeT a) (M.MaybeT b) -> Int
pureRoots (M.PairT a b) = tag (M.tval a) + tag (M.tval b)

countRoots :: M.PairT (M.MaybeT a) (M.MaybeT b) -> M.MemoristM Int
countRoots (M.PairT a b) = (+) <$> (tag <$> M.force a) <*> (tag <$> M.force b)

projectSelected :: Sample -> Views -> (Maybe Int,Maybe Int)
projectSelected s (M.PairT a b) = (selected (leftDemand s) a,selected (rightDemand s) b)
  where
    selected wanted root = if wanted then Just (tag (M.tval root)) else Nothing

rootDemand :: M.Th (M.MaybeT a) -> M.ThD (M.MaybeD b)
rootDemand root = M.Need (case M.tval root of M.NothingT -> M.NothingD; M.JustT _ -> M.JustD M.Skip)

demandSelected :: Sample -> Views -> M.PairD (M.MaybeD Int) (M.MaybeD Bool)
demandSelected s (M.PairT a b) = M.PairD
    (if leftDemand s then rootDemand a else M.Skip)
    (if rightDemand s then rootDemand b else M.Skip)

outerDemand :: Nested -> M.PairD (M.MaybeD (M.MaybeD Int)) (M.MaybeD (M.MaybeD Bool))
outerDemand (M.PairT a b) = M.PairD (rootDemand a) (rootDemand b)

innerDemand :: Nested -> M.PairD (M.MaybeD (M.MaybeD Int)) (M.MaybeD (M.MaybeD Bool))
innerDemand (M.PairT a b) = M.PairD (inside a) (inside b)
  where
    inside root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT middle -> M.JustD (rootDemand middle))

extract :: M.MaybeT a -> M.MemoristM a
extract M.NothingT = error "unexpected empty Maybe"
extract (M.JustT field) = M.force field

intPayload :: Views -> M.MemoristM Int
intPayload (M.PairT a _) = M.force a >>= extract

boolPayload :: Views -> M.MemoristM Int
boolPayload (M.PairT _ b) = do
    value <- M.force b >>= extract
    pure (if value then 1 else 0)

intNestedLeaf :: Nested -> M.MemoristM Int
intNestedLeaf (M.PairT a _) = M.force a >>= extract >>= extract

boolNestedLeaf :: Nested -> M.MemoristM Int
boolNestedLeaf (M.PairT _ b) = do
    value <- M.force b >>= extract >>= extract
    pure (if value then 1 else 0)

pureNested :: Nested -> Int
pureNested (M.PairT a b) = levels (M.tval a) + levels (M.tval b)
  where
    levels M.NothingT = 0
    levels (M.JustT inside) = 1 + tag (M.tval inside)

countNested :: Nested -> M.MemoristM Int
countNested (M.PairT a b) = (+) <$> (M.force a >>= levels) <*> (M.force b >>= levels)
  where
    levels M.NothingT = pure 0
    levels (M.JustT inside) = (1 +) . tag <$> M.force inside

nodeNames :: M.Th (M.MaybeT a) -> M.MemoristM [M.ThName]
nodeNames root = do
    value <- M.force root
    pure (M.tname root : case value of M.NothingT -> []; M.JustT field -> [M.tname field])

viewNames :: Views -> M.MemoristM ([M.ThName],[M.ThName])
viewNames (M.PairT a b) = (,) <$> nodeNames a <*> nodeNames b

nestedNames :: Nested -> M.MemoristM ([M.ThName],[M.ThName])
nestedNames (M.PairT a b) = (,) <$> names a <*> names b
  where
    names root = do
        value <- M.force root
        rest <- case value of M.NothingT -> pure []; M.JustT inside -> nodeNames inside
        pure (M.tname root : rest)

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint a b = all (`notElem` b) a

sharedFresh :: ([M.ThName],[M.ThName]) -> ([M.ThName],[M.ThName]) -> Bool
sharedFresh (a,b) (c,d) = a == b && c == d && disjoint a c
