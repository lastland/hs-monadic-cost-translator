{-# LANGUAGE RankNTypes #-}
module Case (probes) where

import Control.Monad ((>=>))
import qualified Example as Source
import Generated (viewsM, sameM, distinctM, capturedM, nestedM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

type Views = M.PairT (M.PairT Int Bool) (M.PairT Bool Int)
type Fixed t = M.PairT (M.PairT Int t) (M.PairT Bool t)
type Nested a b = M.PairT (M.MaybeT (L.ListT a)) (L.ListT (M.MaybeT (M.PairT b a)))
type NestedD a b = M.PairD (M.MaybeD (L.ListD a)) (L.ListD (M.MaybeD (M.PairD b a)))

genericViews :: forall t. M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int Bool) (M.PairT Bool Int))
genericViews = viewsM

genericCaptured :: forall t. M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int t) (M.PairT Bool t))
genericCaptured = capturedM

probes :: [Probe]
probes =
    [ scalarSource "views/both-roots-one-shared-gate-producer" (sourceRoots Source.views)
        (roots . runViews)
    , scalarSource "views/repeated-roots" (\s -> repeats s * sourceRoots Source.views s)
        (\s -> runViews s >>= repeatedRoots (repeats s))
    , scalarSource "same/both-roots-one-shared-gate-producer" (sourceRoots Source.same)
        (roots . runSame)
    , scalarSource "distinct/equal-RHS-two-gates-producers" (sourceRoots Source.distinct)
        (roots . runDistinct)
    , scalarSource "views/fresh-locals-reused-caller-thunks" (\s -> 2 * sourceRoots Source.views s)
        freshViews
    , demanded "views/root-only" (const rootProjection) (const rootDemand) runViews
    , demanded "views/skip-all-bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (M.fromPure2 genericViews (undefined :: Bool) (undefined :: Int)))
    , demanded "same/skip-all" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip) runSame
    , scalarSource "captured/int-right-fields" sourceFixedInt
        (runFixedInt >=> fixedIntRights)
    , scalarSource "captured/bool-right-fields" sourceFixedBool
        (runFixedBool >=> fixedBoolRights)
    , scalarSource "captured/fresh-locals-reused-costed-caller-thunks" (\s -> 4 * number s)
        freshFixed
    , demanded "captured/one-or-both-fixed-fields" fixedProjection fixedDemand runFixedInt
    , demanded "captured/root-only-bottom-fixed-input" (const rootProjection) (const rootDemand)
        (M.fromPure2 genericCaptured (undefined :: Bool) . count)
    , demanded "nested/partial-left-right-fields" nestedProjection nestedDemand runNested
    , demanded "nested/left-field-only" (\_ -> nestedProjection sampleLeft)
        (\_ -> nestedDemand sampleLeft) runNested
    , demanded "nested/right-field-only" (\_ -> nestedProjection sampleRight)
        (\_ -> nestedDemand sampleRight) runNested
    , demanded "nested/root-only-bottom-count" (const rootProjection)
        (\_ _ -> M.PairD (M.Need (M.PairD M.Skip M.Skip)) (M.Need (M.PairD M.Skip M.Skip)))
        (const (M.fromPure2 nestedM (undefined :: Bool) (undefined :: Int)))
    , demanded "nested/skip-all-bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (M.fromPure2 nestedM (undefined :: Bool) (undefined :: Int)))
    , scalarSource "nested/source-specific-structure" sourceNested
        (runNested >=> observeNestedAll)
    , snapshot "views/root-and-both-field-identities-IntBool-BoolInt" (sameNames . runViews)
    , snapshot "same/root-and-both-field-identities-IntInt-BoolBool" (sameNames . runSame)
    , snapshot "views/root-and-both-field-annotations" (sameAnnotations . runViews)
    , snapshot "distinct/equal-RHS-disjoint-root-and-both-fields" (distinctNames . runDistinct)
    , snapshot "views/fresh-outer-calls-disjoint-locals-same-caller-thunks" freshViewNames
    , snapshot "captured/fixed-field-retains-caller-identity-across-fresh-locals" freshFixedNames
    , snapshot "nested/all-nine-identities-cross-instantiation" (nestedIdentities . runNested)
    , snapshot "nested/fresh-locals-disjoint-nine-identities" freshNestedNames
    , failure "source/views/int-left-bottom" (pure . fst . fst . Source.views () . count)
    , failure "views/int-left-bottom" (runViews >=> intLeft)
    , failure "source/views/bool-right-bottom-consumed" (pure . boolInt . snd . fst . Source.views () . count)
    , failure "views/bool-right-bottom-consumed" (runViews >=> boolRight)
    , failure "source/views/bool-left-bottom-consumed" (pure . boolInt . fst . snd . Source.views () . count)
    , failure "views/bool-left-bottom-consumed" (runViews >=> boolLeft)
    , failure "source/views/int-right-bottom" (pure . snd . snd . Source.views () . count)
    , failure "views/int-right-bottom" (runViews >=> intRight)
    , failure "same/bool-left-bottom-consumed" (runSame >=> sameBoolLeft)
    , failure "same/bool-right-bottom-consumed" (runSame >=> sameBoolRight)
    , failure "source/views/root-bottom" (const (pure (sourceRootFailure Source.views)))
    , failure "views/root-bottom" (const (roots (M.fromPure2 genericViews (undefined :: Bool) (-1 :: Int))))
    , failure "views/demanded-bottom-count" (const (roots (M.fromPure2 genericViews (undefined :: Bool) (undefined :: Int))))
    , failure "source/captured/fixed-bool-bottom-consumed" (const (pure (sourceFixedBoolBottom ())))
    , failure "captured/fixed-bool-bottom-consumed" (const (M.fromPure2 genericCaptured (undefined :: Bool) (0 :: Int) >>= fixedBoolRights))
    , failure "source/nested/bool-left-bottom-consumed" (pure . sourceNestedBoolLeft . count)
    , failure "nested/bool-left-bottom-consumed" (runNested >=> nestedBoolLeft)
    , failure "source/nested/bool-right-bottom-consumed" (pure . sourceNestedBoolRight . count)
    , failure "nested/bool-right-bottom-consumed" (runNested >=> nestedBoolRight)
    , failure "nested/left-producer-bottom" (const (M.fromPure2 nestedM () (-1 :: Int) >>= observeNestedLeft))
    , failure "nested/right-producer-bottom" (const (M.fromPure2 nestedM () (-1 :: Int) >>= observeNestedRight))
    ]

count :: Sample -> Int
count s = abs (number s) + 1

sampleLeft, sampleRight :: Sample
sampleLeft = Sample 0 0 [] 0 1 True False 0
sampleRight = Sample 0 0 [] 0 1 False True 0

runViews :: Sample -> M.MemoristM Views
runViews s = M.fromPure2 genericViews (undefined :: Bool) (count s)

runSame :: Sample -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Bool Bool))
runSame s = M.fromPure2 sameM (undefined :: Bool) (count s)

runDistinct :: Sample -> M.MemoristM Views
runDistinct s = M.fromPure2 distinctM (undefined :: Bool) (count s)

runFixedInt :: Sample -> M.MemoristM (Fixed Int)
runFixedInt s = M.fromPure2 genericCaptured (number s) (count s)

runFixedBool :: Sample -> M.MemoristM (Fixed Bool)
runFixedBool s = M.fromPure2 genericCaptured (leftDemand s) (count s)

runNested :: Sample -> M.MemoristM (M.PairT (Nested Int Bool) (Nested Bool Int))
runNested s = M.fromPure2 nestedM (undefined :: Bool) (count s)

pairTag :: (a,b) -> Int
pairTag (_,_) = 1

sourceRoots :: (Bool -> Int -> ((a,b),(c,d))) -> Sample -> Int
sourceRoots make s = case make undefined (count s) of
    (left,right) -> pairTag left + pairTag right

sourceRootFailure :: (Bool -> Int -> ((a,b),(c,d))) -> Int
sourceRootFailure make = case make undefined (-1) of (left,_) -> pairTag left

roots :: M.MemoristM (M.PairT (M.PairT a b) (M.PairT c d)) -> M.MemoristM Int
roots program = program >>= repeatedRoots 1

repeatedRoots :: Int -> M.PairT (M.PairT a b) (M.PairT c d) -> M.MemoristM Int
repeatedRoots times value@(M.PairT left right)
    | times <= 0 = pure 0
    | otherwise = do
        M.PairT _ _ <- M.force left
        M.PairT _ _ <- M.force right
        rest <- repeatedRoots (times - 1) value
        pure (2 + rest)

rootProjection :: M.PairT (M.PairT a b) (M.PairT c d) -> Int
rootProjection (M.PairT left right) = tag (M.tval left) + tag (M.tval right)
  where tag (M.PairT _ _) = 1

rootDemand :: M.PairT (M.PairT a b) (M.PairT c d) -> M.PairD (M.PairD a b) (M.PairD c d)
rootDemand _ = M.PairD (M.Need (M.PairD M.Skip M.Skip)) (M.Need (M.PairD M.Skip M.Skip))

freshViews :: Sample -> M.MemoristM Int
freshViews s = do
    input <- M.lazy (pure (undefined :: Bool))
    n <- M.lazy (M.tick >> pure (count s))
    first <- genericViews input n >>= repeatedRoots 1
    second <- genericViews input n >>= repeatedRoots 1
    pure (first + second)

boolInt :: Bool -> Int
boolInt value = if value then 1 else 0

sourceFixedInt :: Sample -> Int
sourceFixedInt s = case Source.captured (number s) (count s) of
    ((_,a),(_,b)) -> a + b

sourceFixedBool :: Sample -> Int
sourceFixedBool s = case Source.captured (leftDemand s) (count s) of
    ((_,a),(_,b)) -> boolInt a + boolInt b

sourceFixedBoolBottom :: () -> Int
sourceFixedBoolBottom () = case Source.captured (undefined :: Bool) 0 of
    ((_,a),_) -> boolInt a

fixedIntRights :: Fixed Int -> M.MemoristM Int
fixedIntRights (M.PairT first second) = do
    M.PairT _ left <- M.force first
    M.PairT _ right <- M.force second
    a <- M.force left
    b <- M.force right
    pure (a + b)

fixedBoolRights :: Fixed Bool -> M.MemoristM Int
fixedBoolRights (M.PairT first second) = do
    M.PairT _ left <- M.force first
    M.PairT _ right <- M.force second
    a <- M.force left
    b <- M.force right
    pure (boolInt a + boolInt b)

freshFixed :: Sample -> M.MemoristM Int
freshFixed s = do
    input <- M.lazy (M.tick >> pure (number s))
    n <- M.lazy (M.tick >> pure (count s))
    first <- genericCaptured input n >>= fixedIntRights
    second <- genericCaptured input n >>= fixedIntRights
    pure (first + second)

fixedProjection :: Sample -> Fixed Int -> (Maybe Int, Maybe Int)
fixedProjection s (M.PairT left right) = (one (leftDemand s) left, one (rightDemand s) right)
  where
    one False _ = Nothing
    one True root = case M.tval root of M.PairT _ field -> Just (M.tval field)

fixedDemand :: Sample -> Fixed Int -> M.PairD (M.PairD Int Int) (M.PairD Bool Int)
fixedDemand s _ = M.PairD (one (leftDemand s)) (one (rightDemand s))
  where one want = if want then M.Need (M.PairD M.Skip (M.Need (number s))) else M.Skip

intLeft, boolRight, boolLeft, intRight :: Views -> M.MemoristM Int
intLeft (M.PairT left _) = do M.PairT field _ <- M.force left; M.force field
boolRight (M.PairT left _) = do M.PairT _ field <- M.force left; boolInt <$> M.force field
boolLeft (M.PairT _ right) = do M.PairT field _ <- M.force right; boolInt <$> M.force field
intRight (M.PairT _ right) = do M.PairT _ field <- M.force right; M.force field

sameBoolLeft, sameBoolRight :: M.PairT (M.PairT Int Int) (M.PairT Bool Bool) -> M.MemoristM Int
sameBoolLeft (M.PairT _ right) = do M.PairT field _ <- M.force right; boolInt <$> M.force field
sameBoolRight (M.PairT _ right) = do M.PairT _ field <- M.force right; boolInt <$> M.force field

names :: M.Th (M.PairT a b) -> [M.ThName]
names root = case M.tval root of M.PairT left right -> [M.tname root, M.tname left, M.tname right]

annotations :: M.Th (M.PairT a b) -> [M.Annot]
annotations root = case M.tval root of M.PairT left right -> [M.tannot root, M.tannot left, M.tannot right]

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs ys = all (`notElem` ys) xs

sameNames :: M.MemoristM (M.PairT (M.PairT a b) (M.PairT c d)) -> (Bool,Bool)
sameNames action = case M.value (M.eval1 action) of
    M.PairT left right -> (names left == names right, unique (names left))

sameAnnotations :: M.MemoristM (M.PairT (M.PairT a b) (M.PairT c d)) -> Bool
sameAnnotations action = case M.value (M.eval1 action) of
    M.PairT left right -> annotations left == annotations right

distinctNames :: M.MemoristM (M.PairT (M.PairT a b) (M.PairT c d)) -> (Bool,Bool)
distinctNames action = case M.value (M.eval1 action) of
    M.PairT left right -> (disjoint (names left) (names right), unique (names left ++ names right))

freshViewNames :: Sample -> (Bool,Bool)
freshViewNames s = case M.value (M.eval1 action) of
    (M.PairT a b, M.PairT c d) ->
        (names a == names b && names c == names d, disjoint (names a) (names c))
  where
    action = do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (count s))
        first <- viewsM input n
        second <- viewsM input n
        pure (first,second)

freshFixedNames :: Sample -> (Bool,Bool,Bool,Bool)
freshFixedNames s = case M.value (M.eval1 action) of
    (inputName,M.PairT a b,M.PairT c d) ->
        let local root = take 2 (names root)
            fixed root = last (names root)
        in (names a == names b && names c == names d,
            disjoint (local a) (local c), all ((== inputName) . fixed) [a,c],
            fixed a == fixed b && fixed c == fixed d)
  where
    action = do
        input <- M.lazy (pure (number s))
        n <- M.lazy (pure (count s))
        first <- capturedM input n
        second <- capturedM input n
        pure (M.tname input,first,second)

nestedShape :: (Maybe [a], [Maybe (b,a)]) -> Int
nestedShape (left,right) = leftTag left + rightTag right
  where
    leftTag Nothing = 0
    leftTag (Just []) = 1
    leftTag (Just (_:_)) = error "unexpected source nonempty left"
    rightTag [Just (_, _)] = 1
    rightTag _ = error "unexpected source right shape"

sourceNested :: Sample -> Int
sourceNested s = case Source.nested (undefined :: Bool) (count s) of
    (left,right) -> nestedShape left + nestedShape right

observeNestedAll :: M.PairT (Nested Int Bool) (Nested Bool Int) -> M.MemoristM Int
observeNestedAll (M.PairT first second) = do
    M.PairT l r <- M.force first
    M.PairT l' r' <- M.force second
    a <- leftShape l
    b <- rightShape r
    c <- leftShape l'
    d <- rightShape r'
    pure (a+b+c+d)

leftShape :: M.Th (M.MaybeT (L.ListT a)) -> M.MemoristM Int
leftShape root = do
    list <- justPayload root
    value <- M.force list
    case value of
        L.NilT -> pure 1
        L.ConsT _ _ -> error "unexpected nonempty left observation"

rightShape :: M.Th (L.ListT (M.MaybeT (M.PairT a b))) -> M.MemoristM Int
rightShape root = do
    (item,rest) <- consFields root
    pair <- justPayload item
    M.PairT _ _ <- M.force pair
    value <- M.force rest
    case value of
        L.NilT -> pure 1
        L.ConsT _ _ -> error "unexpected nonempty tail observation"

justPayload :: M.Th (M.MaybeT a) -> M.MemoristM (M.Th a)
justPayload root = do
    value <- M.force root
    case value of
        M.JustT payload -> pure payload
        M.NothingT -> error "unexpected Nothing observation"

consFields :: M.Th (L.ListT a) -> M.MemoristM (M.Th a,M.Th (L.ListT a))
consFields root = do
    value <- M.force root
    case value of
        L.ConsT item rest -> pure (item,rest)
        L.NilT -> error "unexpected Nil observation"

nestedProjection :: Sample -> M.PairT (Nested Int Bool) (Nested Bool Int) -> ((Maybe Int, Maybe Int),(Maybe Int, Maybe Int))
nestedProjection s (M.PairT first second) = (one first, one second)
  where
    one root = case M.tval root of
        M.PairT left right ->
            (if leftDemand s then Just (leftTag (M.tval left)) else Nothing,
             if rightDemand s then Just (rightTag (M.tval right)) else Nothing)
    leftTag M.NothingT = 0
    leftTag (M.JustT list) = case M.tval list of
        L.NilT -> 1
        L.ConsT _ _ -> error "unexpected observed left shape"
    rightTag (L.ConsT item rest) = case (M.tval item,M.tval rest) of
        (M.JustT pair,L.NilT) -> case M.tval pair of M.PairT _ _ -> 1
        _ -> error "unexpected observed right item"
    rightTag L.NilT = error "unexpected observed empty right"

nestedDemand :: Sample -> M.PairT (Nested Int Bool) (Nested Bool Int) -> M.PairD (NestedD Int Bool) (NestedD Bool Int)
nestedDemand s _ = M.PairD (M.Need one) (M.Need one)
  where
    one = M.PairD
        (if leftDemand s then M.Need (M.JustD (M.Need L.NilD)) else M.Skip)
        (if rightDemand s then M.Need (L.ConsD
            (M.Need (M.JustD (M.Need (M.PairD M.Skip M.Skip)))) (M.Need L.NilD)) else M.Skip)

nestedNames :: M.Th (Nested a b) -> [M.ThName]
nestedNames root = case M.tval root of
    M.PairT left right -> case (M.tval left,M.tval right) of
        (M.JustT nil,L.ConsT item rest) -> case M.tval item of
            M.JustT pair -> names root ++ [M.tname nil,M.tname item,M.tname rest] ++ names pair
            M.NothingT -> error "unexpected identity item"
        _ -> error "unexpected identity shape"

nestedIdentities :: M.MemoristM (M.PairT (Nested Int Bool) (Nested Bool Int)) -> (Bool,Bool,Bool)
nestedIdentities action = case M.value (M.eval1 action) of
    M.PairT left right -> (nestedNames left == nestedNames right,
                          unique (nestedNames left), length (nestedNames left) == 9)

freshNestedNames :: Sample -> Bool
freshNestedNames s = case M.value (M.eval1 action) of
    (M.PairT first _,M.PairT second _) -> disjoint (nestedNames first) (nestedNames second)
  where
    action = do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (count s))
        first <- nestedM input n
        second <- nestedM input n
        pure (first,second)

sourceNestedBoolLeft :: Int -> Int
sourceNestedBoolLeft n = case Source.nested () n of
    ((_,[Just (value,_)]),_) -> boolInt value
    _ -> error "unexpected source nested bool left"

sourceNestedBoolRight :: Int -> Int
sourceNestedBoolRight n = case Source.nested () n of
    (_,(_,[Just (_,value)])) -> boolInt value
    _ -> error "unexpected source nested bool right"

nestedBoolLeft, nestedBoolRight :: M.PairT (Nested Int Bool) (Nested Bool Int) -> M.MemoristM Int
nestedBoolLeft (M.PairT first _) = do
    M.PairT _ right <- M.force first
    (item,_) <- consFields right
    pair <- justPayload item
    M.PairT field _ <- M.force pair
    boolInt <$> M.force field
nestedBoolRight (M.PairT _ second) = do
    M.PairT _ right <- M.force second
    (item,_) <- consFields right
    pair <- justPayload item
    M.PairT _ field <- M.force pair
    boolInt <$> M.force field

observeNestedLeft, observeNestedRight :: M.PairT (Nested Int Bool) (Nested Bool Int) -> M.MemoristM Int
observeNestedLeft (M.PairT first _) = do M.PairT left _ <- M.force first; leftShape left
observeNestedRight (M.PairT first _) = do M.PairT _ right <- M.force first; rightShape right
