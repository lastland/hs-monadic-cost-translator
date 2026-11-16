module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

type PairFn = M.Th Int -> M.MemoristM (M.PairT Int Int)
type ListFn = M.Th Int -> M.Th (L.ListT Int) ->
    M.MemoristM (L.ListT (M.PairT Int Int))
type Mask = (Bool, Bool)

pairFunctions :: [(String, Int -> (Int, Int), PairFn)]
pairFunctions =
    [ ("alias", Source.alias, aliasM)
    , ("case-alias", Source.caseAlias, caseAliasM)
    , ("guard-alias", Source.guardAlias, guardAliasM)
    , ("separate", Source.separate, separateM)
    , ("local", Source.localPair, localPairM)
    , ("case-local", Source.caseLocal, caseLocalM)
    , ("case-annotated", Source.caseAnnotated, caseAnnotatedM)
    , ("guard-local", Source.guardLocal, guardLocalM)
    , ("guard-annotated", Source.guardAnnotated, guardAnnotatedM)
    , ("computed-case", Source.computedCase, computedCaseM)
    , ("computed-guard", Source.computedGuard, computedGuardM)
    , ("returned-shared", Source.returnedShared, returnedSharedM)
    ]

listFunctions :: [(String, Int -> [Int] -> [(Int, Int)], ListFn)]
listFunctions =
    [ ("captured", Source.captured, capturedM)
    , ("computed-capture", Source.computedCapture, computedCaptureM)
    , ("specialized", Source.specialized, specializedM)
    , ("nested-specialized", Source.nestedSpecialized, nestedSpecializedM)
    ]

-- Every sample observes all four masks. The fifth observation also exercises
-- the shared generator's demand fields, without changing the probe catalog.
masks :: [(String, Mask)]
masks = [("none", (False, False)), ("left", (True, False)),
         ("right", (False, True)), ("both", (True, True))]

sampleMasks :: [(String, Sample -> Mask)]
sampleMasks = [(label, const mask) | (label, mask) <- masks] ++
    [("sample", \s -> (leftDemand s, rightDemand s))]

projectPair :: Mask -> M.PairT Int Int -> (Maybe Int, Maybe Int)
projectPair (left, right) (M.PairT a b) =
    (if left then Just (M.tval a) else Nothing,
     if right then Just (M.tval b) else Nothing)

pairDemand :: Mask -> M.PairT Int Int -> M.PairD Int Int
pairDemand (left, right) (M.PairT a b) = M.PairD
    (if left then M.Need (M.tval a) else M.Skip)
    (if right then M.Need (M.tval b) else M.Skip)

-- Observer traversal is not a source function. Reversing the forcing order
-- and repeating the same thunks must not invent source calls.
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

repeatedPair :: PairFn -> (Sample -> Int) -> (Sample -> Mask) ->
    Sample -> M.MemoristM Int
repeatedPair make input mask s = do
    pair <- M.fromPure1 make (input s)
    values <- replicateM (repeats s) (observePair s (mask s) pair)
    pure (sum values)

-- Normalize identities: only equality relationships and caller-level name
-- consumption matter, not the representation's concrete ancestral names.
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

callerAllocations :: M.ThName -> Maybe Int
callerAllocations next = lookup next
    (zip (take 64 (iterate M.nnext M.nzero)) [0..])

pairProbes :: (String, Int -> (Int, Int), PairFn) -> [Probe]
pairProbes (label, source, make) =
    [snapshot (label ++ "/finite-identities-and-allocations")
        (\s -> pairResources make (pure (number s))),
     snapshot (label ++ "/bottom-identities-and-allocations")
        (\_ -> pairResources make (error "unused pair input demanded"))] ++
    concat
    [ [ demanded (label ++ "/demand/" ++ maskLabel)
            (\s -> projectPair (mask s)) (\s -> pairDemand (mask s))
            (\s -> M.fromPure1 make (number s))
      , scalarSource (label ++ "/repeat/" ++ maskLabel)
            (\s -> repeats s * observePure (mask s) (source (number s)))
            (repeatedPair make number mask)
      ]
    | (maskLabel, mask) <- sampleMasks
    ] ++
    [demanded (label ++ "/bottom-skipped-preparation")
        (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (\_ -> M.fromPure1 make (undefined :: Int))] ++
    [failure (label ++ "/bottom-demand/" ++ maskLabel)
        (repeatedPair make (const undefined) (const mask))
    | (maskLabel, mask) <- drop 1 masks]

inputs :: Sample -> [Int]
inputs s = take (width s) (numbers s ++ cycle [other s, number s])

projectList :: Mask -> L.ListT (M.PairT Int Int) -> [(Maybe Int, Maybe Int)]
projectList _ L.NilT = []
projectList mask (L.ConsT h t) =
    projectPair mask (M.tval h) : projectList mask (M.tval t)

listDemand :: Mask -> L.ListT (M.PairT Int Int) -> L.ListD (M.PairD Int Int)
listDemand _ L.NilT = L.NilD
listDemand mask (L.ConsT h t) = L.ConsD
    (M.Need (pairDemand mask (M.tval h)))
    (M.Need (listDemand mask (M.tval t)))

observeList :: Sample -> Mask -> L.ListT (M.PairT Int Int) -> M.MemoristM Int
observeList _ _ L.NilT = pure 0
observeList s mask (L.ConsT h t) = do
    pair <- M.force h
    here <- observePair s mask pair
    rest <- M.force t >>= observeList s mask
    pure (here + rest)

repeatedList :: ListFn -> (Sample -> Int) -> (Sample -> [Int]) ->
    (Sample -> Mask) -> Sample -> M.MemoristM Int
repeatedList make input items mask s = do
    output <- M.fromPure2 make (input s) (items s)
    values <- replicateM (repeats s) (observeList s (mask s) output)
    pure (sum values)

inputNames :: L.ListT Int -> [M.ThName]
inputNames L.NilT = []
inputNames (L.ConsT h t) = M.tname h : inputNames (M.tval t)

outputNames :: L.ListT (M.PairT Int Int) -> [(M.ThName, M.ThName)]
outputNames L.NilT = []
outputNames (L.ConsT h t) = case M.tval h of
    M.PairT a b -> (M.tname a, M.tname b) : outputNames (M.tval t)

captureResources :: ListFn -> Int -> [Int] -> (Int, [Bool], [Bool], Bool)
captureResources make input items = M.value (M.eval1 program)
  where
    program = do
        x <- M.lazy (pure input)
        xs <- M.reprM items
        txs <- M.lazy (pure xs)
        output <- make x txs
        let names = outputNames output
            captures = map fst names
            sameCapture = case captures of
                [] -> []
                first : rest -> map (== first) rest
        pure (length names, sameCapture, map (== M.tname x) captures,
              map snd names == inputNames xs)

listProbes :: (String, Int -> [Int] -> [(Int, Int)], ListFn) -> [Probe]
listProbes (label, source, make) =
    [snapshot (label ++ "/" ++ shape ++ "/capture-and-item-identities")
        (\s -> captureResources make (number s) (items s))
    | (shape, items) <- [("sample-width", inputs), ("empty", const []),
                        ("multi-output", \s -> [number s, other s, number s + 1])]] ++
    concat
    [ [ demanded (label ++ "/" ++ shape ++ "/demand/" ++ maskLabel)
            (\s -> projectList (mask s)) (\s -> listDemand (mask s))
            (\s -> M.fromPure2 make (number s) (items s))
      , scalarSource (label ++ "/" ++ shape ++ "/repeat/" ++ maskLabel)
            (\s -> repeats s * sum
                (map (observePure (mask s)) (source (number s) (items s))))
            (repeatedList make number items mask)
      ]
    | (shape, items) <- [("sample-width", inputs), ("empty", const []),
                        ("multi-output", \s -> [number s, other s, number s + 1])]
    , (maskLabel, mask) <- sampleMasks
    ] ++
    [ demanded (label ++ "/bottom-capture-skipped")
        (\_ -> projectList (False, True)) (\_ -> listDemand (False, True))
        (\s -> M.fromPure2 make (undefined :: Int) [number s, other s])
    , demanded (label ++ "/bottom-items-skipped")
        (\_ -> projectList (True, False)) (\_ -> listDemand (True, False))
        (\s -> M.fromPure2 make (number s) [undefined, undefined :: Int])
    , demanded (label ++ "/empty-skips-bottom-capture")
        (\_ -> projectList (True, True)) (\_ -> listDemand (True, True))
        (\_ -> M.fromPure2 make (undefined :: Int) ([] :: [Int]))
    , failure (label ++ "/bottom-capture-demanded")
        (repeatedList make (const undefined) (const [0, 1]) (const (True, False)))
    , failure (label ++ "/bottom-item-demanded")
        (repeatedList make number (const [undefined, undefined]) (const (False, True)))
    , failure (label ++ "/bottom-list-spine")
        (repeatedList make number (const undefined) (const (False, False)))
    ]

probes :: [Probe]
probes =
    [ scalarSource "work/direct" (\s -> Source.work (number s))
        (\s -> M.fromPure1 workM (number s))
    , scalarSource "keep/direct" (\s -> Source.keep (number s))
        (\s -> M.fromPure1 keepM (number s))
    , failure "work/bottom" (\_ -> M.fromPure1 workM (undefined :: Int))
    , failure "keep/bottom" (\_ -> M.fromPure1 keepM (undefined :: Int))
    ] ++ concatMap pairProbes pairFunctions ++
    concatMap listProbes listFunctions ++
    concat
    [ [ demanded ("pair/direct/" ++ maskLabel)
            (\s -> projectPair (mask s)) (\s -> pairDemand (mask s))
            (\s -> M.fromPure2 pairM (number s) (other s))
      , scalarSource ("pair/direct-repeat/" ++ maskLabel)
            (\s -> repeats s * observePure (mask s)
                (Source.pair (number s) (other s)))
            (\s -> do
                pair <- M.fromPure2 pairM (number s) (other s)
                values <- replicateM (repeats s) (observePair s (mask s) pair)
                pure (sum values))
      ]
    | (maskLabel, mask) <- sampleMasks
    ]
