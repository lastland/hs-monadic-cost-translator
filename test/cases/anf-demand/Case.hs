module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport

type PairFn = M.Th Int -> M.MemoristM (M.PairT Int Int)
type BoxPairFn = M.Th BoxT -> M.MemoristM (M.PairT BoxT BoxT)
type Mask = (Bool, Bool)

masks :: [(String, Mask)]
masks = [("none", (False, False)), ("left", (True, False)),
         ("right", (False, True)), ("both", (True, True))]

projectPair :: Mask -> M.PairT Int Int -> (Maybe Int, Maybe Int)
projectPair (left, right) (M.PairT a b) =
    (if left then Just (M.tval a) else Nothing,
     if right then Just (M.tval b) else Nothing)

pairDemand :: Mask -> M.PairT Int Int -> M.PairD Int Int
pairDemand (left, right) (M.PairT a b) = M.PairD
    (if left then M.Need (M.tval a) else M.Skip)
    (if right then M.Need (M.tval b) else M.Skip)

observePair :: Sample -> Mask -> M.PairT Int Int -> M.MemoristM Int
observePair s (left, right) (M.PairT a b) = do
    let get wanted field = if wanted then M.force field else pure 0
    if odd (variant s) then do
        bv <- get right b
        av <- get left a
        pure (av + bv)
    else do
        av <- get left a
        bv <- get right b
        pure (av + bv)

observePure :: Mask -> (Int, Int) -> Int
observePure (left, right) (a, b) =
    (if left then a else 0) + (if right then b else 0)

repeatedPair :: PairFn -> (Sample -> Int) -> Mask -> Sample -> M.MemoristM Int
repeatedPair make input mask s = do
    result <- M.fromPure1 make (input s)
    observations <- replicateM (repeats s) (observePair s mask result)
    pure (sum observations)

callerAllocations :: M.ThName -> Maybe Int
callerAllocations next = lookup next
    (zip (take 64 (iterate M.nnext M.nzero)) [0..])

pairResources :: PairFn -> M.MemoristM Int -> ([Bool], Maybe Int)
pairResources make input =
    let program = do
            x <- M.lazy input
            pair <- make x
            case pair of
                M.PairT a b -> pure
                    [M.tname a == M.tname x, M.tname b == M.tname x,
                     M.tname a == M.tname b]
        (result, next) = M.runM program M.nzero
    in (M.value result, callerAllocations next)

-- Failure probes require a failure, rather than accepting two accidental
-- exceptions as a successful value comparison. The source half preserves the
-- legacy checks that the corresponding source expression fails too.
bottomInt :: String -> (Sample -> Int) ->
    (Sample -> M.MemoristM Int) -> [Probe]
bottomInt label source program =
    [failure (label ++ "/source") (\s -> pure (source s)),
     failure (label ++ "/costed") program]

bottomBool :: String -> (Sample -> Bool) ->
    (Sample -> M.MemoristM Bool) -> [Probe]
bottomBool label source program = bottomInt label
    (fromEnum . source) (fmap fromEnum . program)

boolAgreement :: String -> (Sample -> Bool) ->
    (Sample -> M.MemoristM Bool) -> [Probe]
boolAgreement label source program =
    [boolean label program,
     scalarSource (label ++ "/source-agreement")
        (\s -> fromEnum (source s)) (fmap fromEnum . program)]

pairFunctions :: [(String, Int -> (Int, Int), PairFn, Bool)]
pairFunctions =
    [ ("computed-case", Source.caseComputed, caseComputedM, False)
    , ("computed-guard", Source.guardComputed, guardComputedM, False)
    , ("conditional-case", Source.caseConditional, caseConditionalM, True)
    , ("conditional-guard", Source.guardConditional, guardConditionalM, True)
    , ("bad-case", Source.caseBad, caseBadM, True)
    , ("bad-guard", Source.guardBad, guardBadM, True)
    , ("source-let", Source.sourceLet, sourceLetM, False)
    ]

pairProbes :: (String, Int -> (Int, Int), PairFn, Bool) -> [Probe]
pairProbes (label, source, make, failingFields) =
    [ snapshot (label ++ "/finite-identities-and-allocations")
        (\s -> pairResources make (pure (number s)))
    , snapshot (label ++ "/bottom-identities-and-allocations")
        (\_ -> pairResources make (error "unused pair input demanded"))
    , scalarSource (label ++ "/bottom-input-spine")
        (\_ -> case source undefined of (_, _) -> 0)
        (\_ -> do
            pair <- M.fromPure1 make (undefined :: Int)
            case pair of M.PairT _ _ -> pure 0)
    , demanded (label ++ "/bottom-input-preparation")
        (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (\_ -> M.fromPure1 make (undefined :: Int))
    ] ++ concat
    [ if failingFields && (left || right)
      then bottomInt (label ++ "/demand/" ++ maskLabel)
            (\s -> repeats s * observePure mask (source (number s)))
            (repeatedPair make number mask)
      else
        [ demanded (label ++ "/demand/" ++ maskLabel)
            (\_ -> projectPair mask) (\_ -> pairDemand mask)
            (\s -> M.fromPure1 make (number s))
        , scalarSource (label ++ "/repeat/" ++ maskLabel)
            (\s -> repeats s * observePure mask (source (number s)))
            (repeatedPair make number mask)
        ]
    | (maskLabel, mask@(left, right)) <- masks
    ] ++
    (if failingFields then [] else
        [scalarSource (label ++ "/left-right-left-reuse")
            (\s -> case source (number s) of (a, b) -> a + b + a)
            (\s -> do
                M.PairT a b <- M.fromPure1 make (number s)
                av <- M.force a
                bv <- M.force b
                again <- M.force a
                pure (av + bv + again))]) ++ concat
    [bottomInt (label ++ "/bottom-input-demand/" ++ maskLabel)
        (\s -> repeats s * observePure mask (source undefined))
        (repeatedPair make (const undefined) mask)
    | (maskLabel, mask) <- drop 1 masks]

boxProject :: Bool -> BoxT -> Maybe Int
boxProject deep (BoxT field) = if deep then Just (M.tval field) else Nothing

boxDemand :: Bool -> BoxT -> BoxD
boxDemand deep (BoxT field) = BoxD
    (if deep then M.Need (M.tval field) else M.Skip)

projectBoxPair :: Bool -> Mask -> M.PairT BoxT BoxT ->
    (Maybe (Maybe Int), Maybe (Maybe Int))
projectBoxPair deep (left, right) (M.PairT a b) =
    (if left then Just (boxProject deep (M.tval a)) else Nothing,
     if right then Just (boxProject deep (M.tval b)) else Nothing)

boxPairDemand :: Bool -> Mask -> M.PairT BoxT BoxT -> M.PairD BoxD BoxD
boxPairDemand deep (left, right) (M.PairT a b) = M.PairD
    (if left then M.Need (boxDemand deep (M.tval a)) else M.Skip)
    (if right then M.Need (boxDemand deep (M.tval b)) else M.Skip)

asResources :: BoxPairFn -> ([Bool], Maybe Int)
asResources make =
    let program = do
            field <- M.lazy (error "as-pattern field demanded")
            x <- M.lazy (pure (BoxT field))
            pair <- make x
            case pair of
                M.PairT a b -> pure
                    [M.tname a == M.tname x, M.tname b == M.tname x,
                     M.tname a == M.tname b]
        (result, next) = M.runM program M.nzero
    in (M.value result, callerAllocations next)

asProbes :: (String, Source.Box -> (Source.Box, Source.Box), BoxPairFn) -> [Probe]
asProbes (label, source, make) =
    [ snapshot (label ++ "/whole-input-identities-and-allocations")
        (\_ -> asResources make)
    , scalarSource (label ++ "/source-outer-constructors")
        (\_ -> case source (Source.Box undefined) of
            (Source.Box _, Source.Box _) -> 0)
        (\_ -> do
            pair <- M.fromPure1 make (Source.Box undefined)
            case pair of
                M.PairT a b -> do
                    av <- M.force a
                    bv <- M.force b
                    case (av, bv) of (BoxT _, BoxT _) -> pure 0)
    ] ++
    [demanded (label ++ "/" ++ depth ++ "/" ++ maskLabel)
        (\_ -> projectBoxPair deep mask) (\_ -> boxPairDemand deep mask)
        (\s -> M.fromPure1 make (Source.Box (if deep then number s else undefined)))
    | (depth, deep) <- [("finite-fields", True), ("bottom-fields-skipped", False)]
    , (maskLabel, mask) <- masks] ++
    bottomInt (label ++ "/bottom-subject")
        (\_ -> case source undefined of (_, _) -> 0)
        (\_ -> do
            pair <- M.fromPure1 make (undefined :: Source.Box)
            case pair of M.PairT _ _ -> pure 0) ++
    bottomInt (label ++ "/bottom-field-demanded")
        (\_ -> case source (Source.Box undefined) of
            (Source.Box n, _) -> n)
        (\_ -> do
            M.PairT a _ <- M.fromPure1 make (Source.Box undefined)
            box <- M.force a
            case box of BoxT field -> M.force field)

lazyMatchers :: [(String, Int -> Int, M.Th Int -> M.MemoristM Int)]
lazyMatchers =
    [("case-variable", Source.caseVariable, caseVariableM),
     ("case-wildcard", Source.caseWildcard, caseWildcardM),
     ("guard-variable", Source.guardVariable, guardVariableM),
     ("guard-wildcard", Source.guardWildcard, guardWildcardM)]

literalMatchers :: [(String, Int -> Int, M.Th Int -> M.MemoristM Int)]
literalMatchers =
    [("case-literal", Source.caseLiteral, caseLiteralM),
     ("guard-literal", Source.guardLiteral, guardLiteralM)]

scalarMatchProbes :: (String, Int -> Int, M.Th Int -> M.MemoristM Int) -> [Probe]
scalarMatchProbes (label, source, make) =
    concat
    [[scalarSource (label ++ "/" ++ inputLabel)
        (\s -> source (input s)) (\s -> M.fromPure1 make (input s)),
      snapshot (label ++ "/" ++ inputLabel ++ "/caller-allocations")
        (\s -> callerAllocations (snd
            (M.runM (M.fromPure1 make (input s)) M.nzero)))]
    | (inputLabel, input) <- [("sample", number), ("zero", const 0),
                             ("nonzero", \s -> abs (number s) + 1)]]

constructorProbes :: (String, Source.Box -> Int, M.Th BoxT -> M.MemoristM Int) -> [Probe]
constructorProbes (label, source, make) =
    [ scalarSource (label ++ "/finite-field")
        (\s -> source (Source.Box (number s)))
        (\s -> M.fromPure1 make (Source.Box (number s)))
    , scalarSource (label ++ "/bottom-field-skipped")
        (\_ -> source (Source.Box undefined))
        (\_ -> M.fromPure1 make (Source.Box undefined))
    , snapshot (label ++ "/caller-allocations")
        (\_ -> callerAllocations (snd
            (M.runM (M.fromPure1 make (Source.Box undefined)) M.nzero)))
    ] ++ bottomInt (label ++ "/bottom-subject")
        (\_ -> source undefined)
        (\_ -> M.fromPure1 make (undefined :: Source.Box))

nestedProbes :: (String, (Source.Box, Source.Box) -> Int,
    M.Th (M.PairT BoxT BoxT) -> M.MemoristM Int) -> [Probe]
nestedProbes (label, source, make) =
    concat
    [[scalarSource (label ++ "/" ++ inputLabel)
        (\s -> source (input s)) (\s -> M.fromPure1 make (input s)),
      snapshot (label ++ "/" ++ inputLabel ++ "/caller-allocations")
        (\s -> callerAllocations (snd
            (M.runM (M.fromPure1 make (input s)) M.nzero)))]
    | (inputLabel, input) <-
        [("sample", \s -> (Source.Box (number s), Source.Box (other s))),
         ("full-match", const (Source.Box 0, Source.Box 0)),
         ("skip-second-field", const (Source.Box 1, Source.Box undefined)),
         ("skip-second-constructor", const (Source.Box 1, undefined)),
         ("right-mismatch", const (Source.Box 0, Source.Box 1))]] ++
    concat
    [bottomInt (label ++ "/" ++ inputLabel)
        (const (source input)) (\_ -> M.fromPure1 make input)
    | (inputLabel, input) <-
        [("reach-second-field", (Source.Box 0, Source.Box undefined)),
         ("reach-second-constructor", (Source.Box 0, undefined)),
         ("first-field-bottom", (Source.Box undefined, Source.Box 1)),
         ("first-constructor-bottom", (undefined, Source.Box 1)),
         ("pair-spine-bottom", undefined)]]

recordProbes :: (String, Source.Record -> Int, M.Th RecordT -> M.MemoristM Int) -> [Probe]
recordProbes (label, source, make) =
    concat
    [[scalarSource (label ++ "/" ++ inputLabel)
        (\s -> source (input s)) (\s -> M.fromPure1 make (input s)),
      snapshot (label ++ "/" ++ inputLabel ++ "/caller-allocations")
        (\s -> callerAllocations (snd
            (M.runM (M.fromPure1 make (input s)) M.nzero)))]
    | (inputLabel, input) <-
        [("sample", \s -> Source.Record (number s) (other s)),
         ("full-match", const (Source.Record 0 0)),
         ("source-field-order", const (Source.Record undefined 1)),
         ("later-field-mismatch", const (Source.Record 1 0))]] ++
    concat
    [bottomInt (label ++ "/" ++ inputLabel)
        (const (source input)) (\_ -> M.fromPure1 make input)
    | (inputLabel, input) <-
        [("reach-later-field", Source.Record undefined 0),
         ("first-tested-field-bottom", Source.Record 0 undefined),
         ("record-spine-bottom", undefined)]]

guardProbes :: (String, Int -> Int, M.Th Int -> M.MemoristM Int) -> [Probe]
guardProbes (label, source, make) =
    [scalarSource (label ++ "/skip-later-guard")
        (\s -> source (negate (abs (number s))))
        (\s -> M.fromPure1 make (negate (abs (number s))))] ++
    bottomInt (label ++ "/reach-later-guard")
        (\s -> source (abs (number s) + 1))
        (\s -> M.fromPure1 make (abs (number s) + 1)) ++
    bottomInt (label ++ "/first-guard-subject-bottom")
        (\_ -> source undefined) (\_ -> M.fromPure1 make (undefined :: Int))

strictFunctions :: [(String, Int -> Int, M.Th Int -> M.MemoristM Int)]
strictFunctions =
    [("work", Source.work, workM), ("scalar", Source.scalar, scalarM),
     ("scalar-work", Source.scalarWork, scalarWorkM),
     ("primitive-work", Source.primitiveWork, primitiveWorkM),
     ("conditional-argument", Source.conditionalArg, conditionalArgM),
     ("nested-let-argument", Source.nestedLetArg, nestedLetArgM)]

probes :: [Probe]
probes = concatMap pairProbes pairFunctions ++
    concatMap asProbes
        [("case-as", Source.caseAs, caseAsM), ("guard-as", Source.guardAs, guardAsM)] ++
    concatMap scalarMatchProbes (lazyMatchers ++ literalMatchers) ++
    concat
    [[scalarSource (label ++ "/bottom-subject-skipped")
        (\_ -> source undefined) (\_ -> M.fromPure1 make (undefined :: Int)),
      snapshot (label ++ "/bottom-subject/caller-allocations")
        (\_ -> callerAllocations (snd
            (M.runM (M.fromPure1 make (undefined :: Int)) M.nzero)))]
    | (label, source, make) <- lazyMatchers] ++
    concat [bottomInt (label ++ "/bottom-subject-demanded")
        (\_ -> source undefined) (\_ -> M.fromPure1 make (undefined :: Int))
    | (label, source, make) <- literalMatchers] ++
    concatMap constructorProbes
        [("case-constructor", Source.caseConstructor, caseConstructorM),
         ("guard-constructor", Source.guardConstructor, guardConstructorM)] ++
    concatMap nestedProbes
        [("case-nested", Source.caseNested, caseNestedM),
         ("guard-nested", Source.guardNested, guardNestedM)] ++
    concatMap recordProbes
        [("case-record", Source.caseRecord, caseRecordM),
         ("guard-record", Source.guardRecord, guardRecordM)] ++
    concatMap guardProbes
        [("case-guards", Source.caseGuardSequence, caseGuardSequenceM),
         ("equation-guards", Source.guardSequence, guardSequenceM)] ++
    concat
    [scalarSource (label ++ "/sample") (\s -> source (number s))
        (\s -> M.fromPure1 make (number s)) :
     bottomInt (label ++ "/bottom") (\_ -> source undefined)
        (\_ -> M.fromPure1 make (undefined :: Int))
    | (label, source, make) <- strictFunctions] ++
    [ scalarSource "ignore/unused-bottom"
        (\s -> Source.ignore undefined (number s))
        (\s -> M.fromPure2 ignoreM (undefined :: Int) (number s))
    , scalarSource "add/sample"
        (\s -> Source.add (number s) (other s))
        (\s -> M.fromPure2 addM (number s) (other s))
    , scalarSource "choose/sample"
        (\s -> Source.choose (leftDemand s) (number s) (other s))
        (\s -> M.fromPure3 chooseM (leftDemand s) (number s) (other s))
    , scalarSource "choose/true-skips-false"
        (\s -> Source.choose True (number s) undefined)
        (\s -> M.fromPure3 chooseM True (number s) (undefined :: Int))
    , scalarSource "choose/false-skips-true"
        (\s -> Source.choose False undefined (number s))
        (\s -> M.fromPure3 chooseM False (undefined :: Int) (number s))
    , scalarSource "conditional-work/positive"
        (\s -> Source.conditionalWork (abs (number s) + 1))
        (\s -> M.fromPure1 conditionalWorkM (abs (number s) + 1))
    ] ++
    bottomInt "bad/direct" (\s -> Source.bad (number s))
        (\s -> M.fromPure1 badM (number s)) ++
    bottomInt "ignore/returned-bottom" (\s -> Source.ignore (number s) undefined)
        (\s -> M.fromPure2 ignoreM (number s) (undefined :: Int)) ++
    bottomInt "add/left-bottom" (\s -> Source.add undefined (number s))
        (\s -> M.fromPure2 addM (undefined :: Int) (number s)) ++
    bottomInt "add/right-bottom" (\s -> Source.add (number s) undefined)
        (\s -> M.fromPure2 addM (number s) (undefined :: Int)) ++
    bottomInt "choose/condition-bottom" (\s -> Source.choose undefined (number s) (other s))
        (\s -> M.fromPure3 chooseM (undefined :: Bool) (number s) (other s)) ++
    bottomInt "choose/selected-true-bottom" (\s -> Source.choose True undefined (number s))
        (\s -> M.fromPure3 chooseM True (undefined :: Int) (number s)) ++
    bottomInt "choose/selected-false-bottom" (\s -> Source.choose False (number s) undefined)
        (\s -> M.fromPure3 chooseM False (number s) (undefined :: Int)) ++
    bottomInt "conditional-work/selected-bad"
        (\s -> Source.conditionalWork (negate (abs (number s) + 1)))
        (\s -> M.fromPure1 conditionalWorkM (negate (abs (number s) + 1))) ++
    bottomInt "conditional-work/condition-bottom" (\_ -> Source.conditionalWork undefined)
        (\_ -> M.fromPure1 conditionalWorkM (undefined :: Int)) ++
    concat
    [boolAgreement (label ++ "/sample") (\s -> source (number s))
        (\s -> M.fromPure1 make (number s)) ++
     boolAgreement (label ++ "/positive") (\s -> source (abs (number s) + 1))
        (\s -> M.fromPure1 make (abs (number s) + 1)) ++
     boolAgreement (label ++ "/nonpositive") (\s -> source (negate (abs (number s))))
        (\s -> M.fromPure1 make (negate (abs (number s)))) ++
     bottomBool (label ++ "/bottom") (\_ -> source undefined)
        (\_ -> M.fromPure1 make (undefined :: Int))
    | (label, source, make) <- [("positive", Source.positive, positiveM),
        ("short-and", Source.shortAnd, shortAndM), ("short-or", Source.shortOr, shortOrM)]] ++
    concat
    [boolAgreement (label ++ "/truth-table/" ++ maskLabel)
        (\_ -> source a b) (\_ -> M.fromPure2 make a b)
    | (label, source, make) <- [("and", Source.boolAnd, boolAndM), ("or", Source.boolOr, boolOrM)]
    , (maskLabel, (a, b)) <- masks] ++
    boolAgreement "and/skips-right-bottom" (\_ -> Source.boolAnd False undefined)
        (\_ -> M.fromPure2 boolAndM False (undefined :: Bool)) ++
    boolAgreement "or/skips-right-bottom" (\_ -> Source.boolOr True undefined)
        (\_ -> M.fromPure2 boolOrM True (undefined :: Bool)) ++
    bottomBool "and/left-bottom" (\_ -> Source.boolAnd undefined False)
        (\_ -> M.fromPure2 boolAndM (undefined :: Bool) False) ++
    bottomBool "and/required-right-bottom" (\_ -> Source.boolAnd True undefined)
        (\_ -> M.fromPure2 boolAndM True (undefined :: Bool)) ++
    bottomBool "or/left-bottom" (\_ -> Source.boolOr undefined True)
        (\_ -> M.fromPure2 boolOrM (undefined :: Bool) True) ++
    bottomBool "or/required-right-bottom" (\_ -> Source.boolOr False undefined)
        (\_ -> M.fromPure2 boolOrM False (undefined :: Bool))
