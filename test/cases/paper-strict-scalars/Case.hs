module Case (probes) where

import Control.Monad (replicateM_)
import qualified Example as Source
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

priced :: Int -> a -> M.MemoristM a
priced ticks x = replicateM_ ticks M.tick >> pure x

-- If a spurious force records the use but ignores the value, its tick still
-- changes the shallow cost. Inspecting the value gives a demanded failure.
bad :: String -> M.MemoristM a
bad message = priced 1 (error message)

call2 :: (M.Th a -> M.Th b -> M.MemoristM c) ->
    M.MemoristM a -> M.MemoristM b -> M.MemoristM c
call2 f a b = do
    x <- M.lazy a
    y <- M.lazy b
    f x y

isCons :: L.ListT a -> Bool
isCons L.NilT = False
isCons (L.ConsT _ _) = True

shallowList, fullList :: String -> (Sample -> M.MemoristM (L.ListT Int)) -> Probe
shallowList label = demanded label (const isCons) (const M.minDemand)
fullList label = demanded label (const (M.abstract :: L.ListT Int -> [Int])) (const M.fullDemand)

-- Every inspected input spine cell costs 1 (including nil); each head costs 2.
pricedList :: [Int] -> M.MemoristM (L.ListT Int)
pricedList items = do
    M.tick
    case items of
        [] -> pure L.NilT
        h : t -> L.ConsT <$> M.lazy (priced 2 h) <*> M.lazy (pricedList t)

finiteTake :: Sample -> M.MemoristM (L.ListT Int)
finiteTake s = call2 takeM (priced 1 (number s `rem` 8)) (pricedList (numbers s))

spineDemand :: L.ListT Int -> L.ListD Int
spineDemand L.NilT = L.NilD
spineDemand (L.ConsT _ t) = L.ConsD M.Skip (M.Need (spineDemand (M.tval t)))

partialList :: M.MemoristM Int -> M.MemoristM (L.ListT Int)
partialList headAction = L.ConsT <$> M.lazy headAction <*> M.lazy (bad "list tail")

tailTag :: L.ListT Int -> M.MemoristM Int
tailTag L.NilT = pure 0
tailTag (L.ConsT _ t) = fromEnum . isCons <$> M.force t

takeProbes :: [Probe]
takeProbes =
    [ shallowList "take/zero-skips-failed-spine" $ \_ ->
        call2 takeM (pure 0) (bad "list spine")
    , shallowList "take/two-skips-both-fields" $ \_ ->
        call2 takeM (pure 2) (partialList (bad "list head"))
    , failure "take/two-demands-failed-tail" $ \s ->
        call2 takeM (pure 2) (partialList (pure (number s))) >>= tailTag
    , fullList "take/one-stops-before-failed-tail" $ \s ->
        call2 takeM (pure 1) (partialList (priced 2 (number s)))
    , failure "take/failed-count-even-on-nil" $ \_ ->
        fromEnum . isCons <$> call2 takeM (bad "count") (pure (L.NilT :: L.ListT Int))
    , shallowList "take/finite-outer" finiteTake
    , demanded "take/finite-spine" (const (length . (M.abstract :: L.ListT Int -> [Int])))
        (const spineDemand) finiteTake
    , fullList "take/finite-full" finiteTake
    , boolean "take/original-head-thunk" $ \s -> do
        h <- M.lazy (priced 2 (number s))
        t <- M.lazy (bad "list tail")
        result <- call2 takeM (pure 2) (pure (L.ConsT h t))
        pure $ case result of
            L.ConsT first _ -> M.tname first == M.tname h
            L.NilT -> False
    ]

isJustT :: M.MaybeT a -> Bool
isJustT M.NothingT = False
isJustT (M.JustT _) = True

type Box = M.MaybeT Int

shallowBox, fullBox :: String -> (Sample -> M.MemoristM Box) -> Probe
shallowBox label = demanded label (const isJustT) (const M.minDemand)
fullBox label = demanded label (const (M.abstract :: Box -> Maybe Int)) (const M.fullDemand)

-- An unexpected Nothing is a successful observation, not a manufactured error.
fieldValue :: Box -> M.MemoristM Int
fieldValue M.NothingT = pure 0
fieldValue (M.JustT x) = M.force x

boxProbes :: (String, M.Th Int -> M.MemoristM Box) -> [Probe]
boxProbes (spelling, make) = concat
    [ [ shallowBox (spelling ++ "/" ++ label ++ "/outer") (call input)
      , fullBox (spelling ++ "/" ++ label ++ "/full") (call input)
      ]
    | (label, input) <- [("pure-3", const (pure 3)), ("ticked-3", const (priced 1 3)),
                         ("varying", \s -> priced (repeats s) (number s))]
    ] ++ [failure (spelling ++ "/failed-guard") $ \_ ->
        fromEnum . isJustT <$> (M.lazy (bad "box input") >>= make)]
  where
    call input s = M.lazy (input s) >>= make

guardProbes :: [Probe]
guardProbes =
    [ shallowBox "guard/skipped-test-and-field" (const (guard False))
    , failure "guard/skipped-test-demanded-field" (\_ -> guard False >>= fieldValue)
    , failure "guard/demanded-test" (\_ -> fromEnum . isJustT <$> guard True)
    , shallowBox "guard/finite-outer" finite
    , fullBox "guard/finite-full" finite
    , scalarSource "guard/same-logical-span" (uncurry Source.mappedGuards . distinctInputs) $ \s ->
        let (x, y) = distinctInputs s in call2 mappedGuardsM (priced 1 x) (priced 2 y)
    , shallowBox "quot/zero-skips-capture-and-division" $ \_ ->
        call2 boxQuotM (bad "captured flag") (priced 2 0)
    , fullBox "quot/entered-helper-ignores-zero-divisor" $ \_ ->
        call2 boxQuotM (priced 1 False) (priced 2 0)
    , failure "quot/demanded-division-by-zero" $ \_ ->
        call2 boxQuotM (priced 1 True) (priced 2 0) >>= fieldValue
    , fullBox "quot/negative-skips-helper" $ \s ->
        call2 boxQuotM (bad "captured flag") (priced 2 (-1 - abs (number s)))
    , fullBox "quot/finite-full" $ \s ->
        call2 boxQuotM (priced 1 (leftDemand s)) (priced 2 (1 + abs (number s)))
    ]
  where
    guard flag = call2 guardedM (priced 1 flag) (bad "conditional operand")
    finite s = call2 guardedM (priced 1 (leftDemand s)) (priced 2 (number s))
    distinctInputs s = (2 * abs (number s) + 1, 2 * abs (other s) + 2)

pairProbe :: String -> Bool -> Bool -> (Sample -> M.MemoristM (M.PairT Int Int)) -> Probe
pairProbe label left right = demanded label (const project) (const demand)
  where
    project (M.PairT a b) = (view left a, view right b)
    view wanted t = if wanted then Just (M.tval t) else Nothing
    demand (M.PairT a b) = M.PairD (need left a) (need right b)
    need wanted t = if wanted then M.Need (M.tval t) else M.Skip

suspensionProbes :: [Probe]
suspensionProbes =
    [ pairProbe ("suspended/" ++ label) left right $ \s ->
        call2 suspendedM (priced 2 (number s)) (priced 3 (other s))
    | (label, left, right) <- [("neither", False, False), ("left", True, False),
                               ("right", False, True), ("both", True, True)]
    ] ++
    [ pairProbe "suspended/neither-skips-both-bad-inputs" False False $ \_ ->
        call2 suspendedM (bad "x") (bad "y")
    , pairProbe "suspended/right-skips-local-call" False True $ \s ->
        call2 suspendedM (priced 2 (1 + abs (number s))) (bad "y")
    , pairProbe "suspended/left-guard-skips-local-call" True False $ \s ->
        call2 suspendedM (priced 2 (negate (abs (number s)))) (bad "y")
    , failure "suspended/local-call-demands-y" $ \_ -> do
        M.PairT a _ <- call2 suspendedM (priced 2 1) (bad "y")
        M.force a
    , failure "suspended/right-demands-x" $ \s -> do
        M.PairT _ b <- call2 suspendedM (bad "x") (pure (other s))
        M.force b
    ]

shadow :: (M.Th Int -> M.Th Int -> Box -> a) -> Sample -> M.MemoristM a
shadow project s = do
    outer <- M.lazy (priced 1 (number s))
    inner <- M.lazy (priced 3 (other s))
    input <- M.lazy (priced 2 (M.JustT inner))
    result <- shadowForwardM outer input
    pure (project outer inner result)

shadowProbes :: [Probe]
shadowProbes =
    [ fullBox "shadow/value" (shadow (\_ _ box -> box))
    , demanded "shadow/relayed-thunk-identity" (const id) (const M.fullDemand) (shadow aliases)
    , fullBox "shadow/outer-skips-failed-input" $ \_ ->
        call2 shadowForwardM (priced 1 0) (bad "optional input")
    , fullBox "shadow/missing-inner" $ \_ ->
        call2 shadowForwardM (priced 1 1) (priced 2 M.NothingT)
    , failure "shadow/inner-test-demands-distinct-input" $ \_ -> do
        inner <- M.lazy (bad "shadowed scalar")
        fromEnum . isJustT <$> call2 shadowForwardM (priced 1 1) (priced 2 (M.JustT inner))
    ]
  where
    aliases _ _ M.NothingT = Nothing
    aliases outer inner (M.JustT field) =
        Just (M.tname field == M.tname outer, M.tname field == M.tname inner)

evidenceProbes :: [Probe]
evidenceProbes =
    [ scalarSource "evidence/shadowed-plus-skips-right" (Source.shadowPlus . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= shadowPlusM
    , failure "evidence/shadowed-plus-needs-left" $ \_ ->
        M.lazy (bad "left operand") >>= shadowPlusM
    , shallowBox "evidence/given-num-outer" $ \_ ->
        M.lazy (bad "boxed summand") >>= givenAddM
    , fullBox "evidence/given-num-full" $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= givenAddM
    , failure "evidence/given-num-needs-summand" $ \_ ->
        M.lazy (bad "boxed summand") >>= givenAddM >>= fieldValue
    ]

probes :: [Probe]
probes =
    [ scalarSource "work/ticked-input" (Source.work . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= workM
    ] ++ takeProbes ++ concatMap boxProbes [("boxNext", boxNextM), ("boxPrefix", boxPrefixM)] ++
    guardProbes ++ suspensionProbes ++ shadowProbes ++ evidenceProbes
