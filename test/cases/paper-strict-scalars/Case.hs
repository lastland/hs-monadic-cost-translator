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
    [ boolean "guard/checked-condition" $ \s ->
        M.lazy (priced (repeats s) (leftDemand s)) >>= checkedFlagM
    , failure "guard/checked-condition-demand" $ \_ ->
        fromEnum <$> (M.lazy (bad "annotated flag") >>= checkedFlagM)
    , boolean "guard/irrefutable-alias-skips-bad-input" $ \_ ->
        aliasIdentity False (bad "skipped scalar")
    , boolean "guard/irrefutable-alias-identity" $ \s ->
        aliasIdentity True (priced 2 (number s))
    , failure "guard/irrefutable-alias-entered" $ \_ ->
        fromEnum <$> aliasIdentity True (bad "tested scalar")
    , shallowBox "guard/skipped-test-and-field" (const (guard False))
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
    ] ++
    [ scalarSource (label ++ "/" ++ show n) (const (source n)) $ \s ->
        M.lazy (priced (repeats s) n) >>= translated
    | (label, source, translated) <- [("guard/let", Source.letGuard, letGuardM),
                                      ("guard/variable", Source.variableGuard, variableGuardM)]
    , n <- [3, -2] ]
  where
    aliasIdentity enabled input = do
        x <- M.lazy input
        flag <- M.lazy (priced 1 enabled)
        result <- aliasGuardM flag x
        pure $ case result of
            M.JustT y -> M.tname x == M.tname y
            M.NothingT -> False
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

shortCircuitProbes :: [Probe]
shortCircuitProbes = concat
    [ [ scalar (label ++ "/skipped") $ \_ ->
            call2 translated (priced 1 (not entered)) (bad "skipped RHS")
      , failure (label ++ "/entered-bad") $ \_ ->
            call2 translated (priced 1 entered) (bad "demanded RHS")
      , scalarSource (label ++ "/entered") (source entered . number) $ \s ->
            call2 translated (priced 1 entered) (priced 3 (number s))
      ]
    | (label, entered, source, translated) <-
        [ ("and", True, Source.afterAnd, afterAndM), ("or", False, Source.afterOr, afterOrM)
        , ("nested-and", True, Source.nestedAnd, nestedAndM) ]
    ]

constantOperandProbes :: [Probe]
constantOperandProbes = concat
    [ [ scalarSource (label ++ "/" ++ show n) (const (source n)) $ \s ->
            M.lazy (priced (repeats s) n) >>= translated
      | n <- [-2, 0, 3] ] ++
      [ failure (label ++ "/demands-x") $ \_ -> M.lazy (bad "comparison operand") >>= translated ]
    | (label, source, translated) <-
        [ ("and/left-true", Source.constantAnd, constantAndM)
        , ("or/left-false", Source.constantOr, constantOrM)
        , ("and/right-false", Source.knownAnd, knownAndM)
        , ("or/right-true", Source.knownOr, knownOrM)
        ]
    ]

patternProbes :: [Probe]
patternProbes = concat
    [ [ scalarSource (label ++ "/" ++ show input) (const (source input)) $ \s ->
            M.lazy (priced (repeats s) input) >>= translated
      | input <- [0, 1, -3] ] ++
      [ failure (label ++ "/failed-scrutinee") $ \_ -> M.lazy (bad "scrutinee") >>= translated ]
    | (label, source, translated) <- [("literal", Source.literal, literalM), ("case", Source.inspect, inspectM)]
    ] ++
    [ scalarSource ("pattern/" ++ label) (const (Source.zeroSign n)) $ \s ->
        M.lazy (priced (repeats s) n) >>= zeroSignM
    | (label, n) <- [("negative-zero", -0.0), ("positive-zero", 0.0), ("nonzero", 1)]
    ]

rowProbes :: [Probe]
rowProbes =
    [ scalarSource ("case/wildcard-guard/" ++ show n) (const (Source.caseRows n undefined)) $ \s ->
        call2 caseRowsM (priced (repeats s) n) (bad "wildcard scrutinee")
    | n <- [-2, 0, 3] ] ++
    [ scalarSource ("case/covered-just/" ++ show n) (const (Source.coveredRows n (Just undefined))) $ \s ->
        call2 coveredRowsM (priced (repeats s) n) (M.tick >> (M.JustT <$> M.lazy (bad "unused field")))
    | n <- [-2, 0, 3] ] ++
    [ scalarSource ("equation/swapped-binders/" ++ show n) (Source.equation n . rhs) $ \s ->
        call2 equationM (priced 2 n) (priced 3 (rhs s))
    | n <- [-2, 1] ] ++
    [ failure "case/wildcard-guard-demands-x" $ \_ ->
        call2 caseRowsM (bad "condition") (pure False)
    , scalarSource "case/covered-nothing-skips-guard" (const (Source.coveredRows undefined Nothing)) $ \_ ->
        call2 coveredRowsM (bad "skipped scalar") (priced 2 M.NothingT)
    , shallowBox "case/join-skips-guard-and-field" $ \_ ->
        call2 joinRowsM (bad "skipped guard and field") (priced 1 False)
    , failure "case/join-pattern-miss-demands-field" $ \_ ->
        call2 joinRowsM (bad "field") (pure False) >>= fieldValue
    , failure "case/join-pattern-match-demands-guard" $ \_ ->
        fromEnum . isJustT <$> call2 joinRowsM (bad "guard") (pure True)
    , fullBox "case/join-both-failure-edges" $ \s ->
        call2 joinRowsM (priced (repeats s) (number s)) (priced 2 (leftDemand s))
    ]
  where rhs s = 4 + abs (other s)

payloadProbes :: [Probe]
payloadProbes =
    [ scalar "payload/skip-both-roots" $ \_ -> make (bad "payload input") >> pure 1
    , failure "payload/root-demands-condition" $ \_ -> do
        M.PairT root _ <- make (bad "payload condition")
        PacketT _ _ <- M.force root
        pure 1
    , boolean "payload/roots-and-fields-are-shared" $ \s -> do
        M.PairT a b <- make (priced 2 (number s))
        PacketT x m <- M.force a
        PacketT y n <- M.force b
        pure ([M.tname a, M.tname x, M.tname m] == [M.tname b, M.tname y, M.tname n])
    , scalarSource "payload/both-typed-views" (source . Source.payload . number) $ \s -> do
        M.PairT a b <- make (priced (repeats s) (number s))
        (+) <$> observe a <*> observe b
    , scalar "payload/entered-branch-skips-failed-field" $ \_ -> do
        M.PairT a _ <- call2 payloadWorkM (priced 2 1) (bad "field operand")
        PacketT _ optional <- M.force a
        fromEnum . isJustT <$> M.force optional
    , failure "payload/entered-branch-demands-field" $ \_ -> do
        M.PairT _ b <- call2 payloadWorkM (pure 1) (bad "field operand")
        observe b
    , scalar "payload/other-branch-skips-work" $ \_ -> do
        M.PairT a _ <- call2 payloadWorkM (pure (-1)) (bad "unused operand")
        observe a
    , scalarSource "payload/both-views-pay-for-work-once"
        (\s -> source (Source.payloadWork (1 + abs (number s)) (other s))) $ \s -> do
        M.PairT a b <- call2 payloadWorkM (priced 2 (1 + abs (number s))) (priced 3 (other s))
        (+) <$> observe a <*> observe b
    ]
  where
    make input = M.lazy input >>= payloadM
    observe root = do
        PacketT field optional <- M.force root
        value <- M.force field
        tag <- M.force optional
        pure (value + fromEnum (isJustT tag))
    source packets = case packets of
        (Source.Packet x Nothing, Source.Packet y Nothing) -> x + y
        _ -> error "unexpected source payload"

probes :: [Probe]
probes =
    [ scalarSource "case/direct-alias" (Source.viaCase . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= viaCaseM
    , scalarSource "work/ticked-input" (Source.work . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= workM
    , scalarSource "bang/work-then-add" (Source.bangNext . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= bangNextM
    , failure "bang/failed-work" $ \_ -> M.lazy (bad "work input") >>= bangNextM
    , scalarSource "nested/value-and-cost" (Source.nested . number) $ \s ->
        M.lazy (priced (repeats s) (number s)) >>= nestedM
    , failure "nested/comparison-demands-input" $ \_ -> M.lazy (bad "input") >>= nestedM
    ] ++ patternProbes ++ shortCircuitProbes ++ takeProbes ++ concatMap boxProbes [("boxNext", boxNextM), ("boxPrefix", boxPrefixM)] ++
    guardProbes ++ suspensionProbes ++ shadowProbes ++ evidenceProbes ++ rowProbes ++ payloadProbes ++ constantOperandProbes
