module Case (probes) where

import qualified Example as Source
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "first-int" (\s -> Source.first (input s))
        (\s -> M.fromPure1 firstM (input s))
    , scalarSource "second-bool" (\s -> fromEnum (Source.second (input s)))
        (\s -> fromEnum <$> M.fromPure1 secondM (input s))
    , scalarSource "ignore-bottom" (const (Source.ignore undefined))
        (\_ -> M.fromPure1 ignoreM (undefined :: Source.Bundle))
    , demanded "complete-bundle" (\_ -> project) (\_ -> M.fullDemand)
        (\s -> M.fromPure2 bundleM (number s) (leftDemand s))
    , demanded "partial-constructor" (\_ _ -> "Bundle")
        (\_ _ -> BundleD M.Skip M.Skip M.Skip)
        (\_ -> M.fromPure1 partialM (undefined :: Int))
    , scalar "partial-first-skips-other-fields" $ \s -> do
        b <- M.lazy (M.fromPure1 partialM (number s))
        firstM b
    , demanded "repr-skips-bottom-siblings" (\_ -> pairInt)
        (\_ b -> BundleD M.Skip (M.Need (M.PairD (M.Need (pairInt b)) M.Skip)) M.Skip)
        (\s -> M.reprM (Source.Bundle undefined (number s, undefined) undefined) :: M.MemoristM BundleT)
    , demanded "bool-skips-int" (\_ -> pairBool)
        (\_ b -> BundleD M.Skip (M.Need (M.PairD M.Skip (M.Need (pairBool b)))) M.Skip)
        (\s -> M.reprM (Source.Bundle undefined (undefined, leftDemand s) undefined) :: M.MemoristM BundleT)
    , demanded "shared-input-two-calls" (\_ -> (M.abstract :: M.PairT Int Int -> (Int, Int))) (\_ -> M.fullDemand) $ \s -> do
        b <- M.lazy (M.fromPure2 bundleM (number s) (leftDemand s))
        twiceM b
    , demanded "record-list-selector" (\_ -> (M.abstract :: L.ListT Int -> [Int])) (\_ -> M.fullDemand)
        (\s -> M.fromPure1 itemsM (input s))
    , demanded "record-pair-selector" (\_ -> (M.abstract :: M.PairT Int Bool -> (Int, Bool))) (\_ -> M.fullDemand)
        (\s -> M.fromPure1 choiceM (input s))
    , demanded "record-maybe-selector" (\_ -> (M.abstract :: M.MaybeT (L.ListT Int) -> Maybe [Int])) (\_ -> M.fullDemand)
        (\s -> M.fromPure1 nestedM (input s))
    , snapshot "caller-name17-cost1-empty-usage-and-aliases" callerSnapshot
    , snapshot "int-caller-complete-result-annotation" (selectedAnnotation False)
    , snapshot "bool-caller-complete-result-annotation" (selectedAnnotation True)
    , failure "demanded-int-bottom" (\_ ->
        M.fromPure1 firstM (Source.Bundle [] (undefined, True) Nothing))
    , failure "demanded-bool-bottom" (\_ ->
        fromEnum <$> M.fromPure1 secondM (Source.Bundle [] (1, undefined) Nothing))
    , failure "partial-boolean-bottom" $ \_ -> do
        b <- M.lazy (M.fromPure1 partialM (1 :: Int))
        fromEnum <$> secondM b
    , failure "partial-list-bottom" $ \_ -> do
        b <- M.lazy (M.fromPure1 partialM (1 :: Int))
        xs <- itemsM b
        case xs of
            L.NilT -> pure 0
            L.ConsT h _ -> M.force h
    ]

input :: Sample -> Source.Bundle
input s = Source.Bundle (numbers s) (number s, leftDemand s) (Just (reverse (numbers s)))

project :: BundleT -> ([Int], (Int, Bool), Maybe [Int])
project (BundleT xs pair rest) =
    (M.abstract (M.tval xs), M.abstract (M.tval pair), M.abstract (M.tval rest))

pairInt :: BundleT -> Int
pairInt (BundleT _ pair _) = case M.tval pair of M.PairT x _ -> M.tval x

pairBool :: BundleT -> Bool
pairBool (BundleT _ pair _) = case M.tval pair of M.PairT _ flag -> M.tval flag

callerSnapshot :: Sample -> (M.ThName, M.Annot, M.ThName, M.Annot, M.ThName, M.Annot, M.Annot)
callerSnapshot s =
    let caller = M.Th (M.fromInt 17) (number s) (M.Annot (M.Cost 1) M.empty)
        result = M.eval1 $ do
            flag <- M.lazy (pure (leftDemand s))
            bundleM caller flag
        BundleT xs pair rest = M.value result
        L.ConsT headValue _ = M.tval xs
        M.PairT firstValue _ = M.tval pair
        M.JustT nestedList = M.tval rest
        L.ConsT nestedValue _ = M.tval nestedList
    in (M.tname headValue, M.tannot headValue,
        M.tname firstValue, M.tannot firstValue,
        M.tname nestedValue, M.tannot nestedValue, M.annot result)

-- Caller-owned names/costs are observable without normalizing usage sets.
-- The two selectors must retain different scalar evidence and skip siblings.
selectedAnnotation :: Bool -> Sample -> (Int, M.Annot)
selectedAnnotation selectBool s =
    let intField = M.Th (M.fromInt 17) (number s) (M.Annot (M.Cost 1) M.empty)
        flag = M.Th (M.fromInt 19) (leftDemand s) (M.Annot (M.Cost 4) M.empty)
        pair = M.Th (M.fromInt 23) (M.PairT intField flag) (M.Annot (M.Cost 2) M.empty)
        inputBundle = M.Th (M.fromInt 31) (BundleT undefined pair undefined)
            (M.Annot (M.Cost 3) M.empty)
        result = M.eval1
            (if selectBool then fromEnum <$> secondM inputBundle else firstM inputBundle)
    in (M.value result, M.annot result)
