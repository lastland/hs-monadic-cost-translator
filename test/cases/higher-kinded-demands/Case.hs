{-# LANGUAGE DataKinds, TypeFamilies #-}
module Case (probes) where

import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import qualified DemandSupport as D
import Memorist.Data.List (ListT(..), ListD(..))
import ReferenceSupport
import qualified Test.QuickCheck as QC

probes :: [Probe]
probes =
    [ scalarSource "literal-index-kinds" (S.indexKinds . number) (\s -> M.fromPure1 indexKindsM (number s))
    , demanded "forest-finite-demand" (\s -> forestView s (width s) [])
        (\s -> forestDemand s (width s)) (\s -> M.fromPure1 forestM (number s))
    , demanded "nested-finite-demand" (\s -> nestedView s (width s))
        (\s -> nestedDemand s (width s)) (\s -> M.fromPure1 nestedM (other s))
    , demanded "forest-unused-bottom-seed" (\s -> forestView s (width s) [])
        (\s -> forestDemand s (width s)) $ \_ -> do
            seed <- M.lazy (error "unused forest seed")
            forestM seed
    , demanded "nested-unused-bottom-seed" (\s -> nestedView s (width s))
        (\s -> nestedDemand s (width s)) $ \_ -> do
            seed <- M.lazy (error "unused nested seed")
            nestedM seed
    , demanded "tagged-non-type-index" (\_ r -> case (M.abstract r :: S.Higher S.Tagged 'S.Key) of
            S.Higher (S.Tagged n) -> n)
        (\_ -> M.fullDemand) (\s -> M.fromPure1 taggedM (number s) :: M.MemoristM (HigherT TaggedT 'S.Key))
    , demanded "higher-custom-conversion" (\_ r -> case (M.abstract r :: S.Higher S.Layer Int) of
            S.Higher (S.Layer n) -> n)
        (\_ -> M.fullDemand) (\s -> M.reprM (S.Higher (S.Layer (number s))) :: M.MemoristM (HigherT LayerT Int))
    , demanded "higher-custom-skipped-field" (\_ (HigherT layer) -> case M.tval layer of LayerT _ -> "Higher Layer")
        (\_ _ -> HigherD (M.Need (LayerD M.Skip)))
        (\_ -> M.reprM (S.Higher (S.Layer (error "skipped Layer field" :: Int))) :: M.MemoristM (HigherT LayerT Int))
    , demanded "higher-list-conversion" (\_ r -> case (M.abstract r :: S.Higher [] Int) of S.Higher xs -> xs)
        (\_ -> M.fullDemand) (\s -> M.reprM (S.Higher (take (width s) (numbers s))) :: M.MemoristM (HigherT ListT Int))
    , demanded "higher-maybe-conversion" (\_ r -> case (M.abstract r :: S.Higher Maybe Int) of S.Higher x -> x)
        (\_ -> M.fullDemand) (\s -> M.reprM (S.Higher (if leftDemand s then Just (other s) else Nothing)) :: M.MemoristM (HigherT M.MaybeT Int))
    , demanded "higher-constructor-argument-conversion" (\_ r -> case (M.abstract r :: S.Higher S.Fix []) of S.Higher x -> fixShape x)
        (\_ -> M.fullDemand) (\s -> M.reprM (S.Higher (finiteFix (width s) (variant s `mod` 3))) :: M.MemoristM (HigherT FixT ListT))
    , demanded "nested-recursive-conversion" (\_ r -> nestShape (M.abstract r :: S.Nest []))
        (\_ -> M.fullDemand) (\s -> M.reprM (finiteNest (width s) (variant s `mod` 3)) :: M.MemoristM (NestT ListT))
    , demanded "mixed-non-regular-conversion" (\_ r -> mixedShape (M.abstract r :: S.Mixed Maybe Int))
        (\_ -> M.fullDemand) (\s -> M.reprM (mixedInput s) :: M.MemoristM (MixedT M.MaybeT Int))
    , generatedDemands $ D.demandProbe "generated-fix-demands"
        (\s -> M.fromPure1 forestM (number s)) genFixDemand shrinkFixDemand
        (D.toDemandCodec fixPlan (D.constructorBudget "In"))
    , generatedDemands $ D.demandProbe "arbitrary-fix-demands"
        (\s -> M.fromPure1 forestM (number s))
        (\budget tree -> QC.resize budget (M.arbDemandFrom tree)) shrinkFixDemand
        (D.toDemandCodec fixPlan (D.constructorBudget "In"))
    , generatedDemands $ D.demandProbe "zero-budget-skips-bottom-field"
        (\_ -> pure (InT (error "zero-budget field evaluated")) :: M.MemoristM (FixT ListT))
        (\_ -> genFixDemand 0) shrinkFixDemand
        (D.toDemandCodec fixPlan (D.constructorBudget "In"))
    , generatedDemands $ D.demandProbe "generated-nest-demands"
        (\s -> M.fromPure1 nestedM (other s)) genNestDemand shrinkNestDemand
        (D.toDemandCodec nestPlan (D.constructorBudget "Nest"))
    , generatedDemands $ D.demandProbe "generated-higher-custom-demands"
        (\s -> M.reprM (S.Higher (S.Layer (number s))) :: M.MemoristM (HigherT LayerT Int))
        genHigherDemand shrinkHigherDemand
        (D.toDemandCodec (higherPlan layerPlan) higherBudget)
    , generatedDemands $ D.demandProbe "generated-higher-constructor-demands"
        (\s -> M.reprM (S.Higher (finiteFix (width s) (variant s `mod` 3))) :: M.MemoristM (HigherT FixT ListT))
        genHigherDemand shrinkHigherDemand
        (D.toDemandCodec (higherPlan fixPlan) higherBudget)
    , generatedDemands $ D.demandProbe "generated-higher-index-demands"
        (\s -> M.fromPure1 taggedM (number s) :: M.MemoristM (HigherT TaggedT 'S.Key))
        genHigherDemand shrinkHigherDemand
        (D.toDemandCodec (higherPlan taggedPlan) higherBudget)
    , generatedDemands $ D.demandProbe "generated-mixed-demands"
        (\s -> M.reprM (mixedInput s) :: M.MemoristM (MixedT M.MaybeT Int))
        genMixedDemand shrinkMixedDemand
        (D.toDemandCodec (mixedPlan D.intPlan) mixedBudget)
    ]

forestView :: Sample -> Int -> [Bool] -> FixT ListT -> [[Bool]]
forestView s depth path (InT xs) = path :
    (if depth > 0 && leftDemand s then forestView s (depth - 1) (path ++ [False]) left else []) ++
    (if depth > 0 && rightDemand s then forestView s (depth - 1) (path ++ [True]) right else [])
  where
    ConsT l rest = M.tval xs
    ConsT r _ = M.tval rest
    left = M.tval l
    right = M.tval r

forestDemand :: Sample -> Int -> FixT ListT -> FixD ListD
forestDemand s depth (InT xs)
    | depth <= 0 || not (leftDemand s || rightDemand s) = InD M.Skip
    | otherwise = case M.tval xs of
        ConsT l rest -> case M.tval rest of
            ConsT r _ -> InD (M.Need (ConsD
                (if leftDemand s then M.Need (forestDemand s (depth - 1) (M.tval l)) else M.Skip)
                (M.Need (ConsD
                    (if rightDemand s then M.Need (forestDemand s (depth - 1) (M.tval r)) else M.Skip)
                    M.Skip))))
            NilT -> error "forest: missing second child"
        NilT -> error "forest: missing children"

nestedView :: Sample -> Int -> NestT ListT -> [String]
nestedView s depth (NestT optional)
    | depth <= 0 || not (leftDemand s) = ["Nest"]
    | otherwise = case M.tval optional of
        M.NothingT -> ["Nest", "Nothing"]
        M.JustT xs -> case M.tval xs of
            NilT -> ["Nest", "Just", "Nil"]
            ConsT x _ -> ["Nest", "Just", "Cons"] ++
                (if rightDemand s then nestedView s (depth - 1) (M.tval x) else [])

nestedDemand :: Sample -> Int -> NestT ListT -> NestD ListD
nestedDemand s depth (NestT optional)
    | depth <= 0 || not (leftDemand s) = NestD M.Skip
    | otherwise = case M.tval optional of
        M.NothingT -> NestD (M.Need M.NothingD)
        M.JustT xs -> case M.tval xs of
            NilT -> NestD (M.Need (M.JustD (M.Need NilD)))
            ConsT x _ -> NestD (M.Need (M.JustD (M.Need (ConsD
                (if rightDemand s then M.Need (nestedDemand s (depth - 1) (M.tval x)) else M.Skip) M.Skip))))

finiteFix :: Int -> Int -> S.Fix []
finiteFix depth branching = S.In
    (if depth <= 0 then [] else replicate branching (finiteFix (depth - 1) branching))

fixShape :: S.Fix [] -> String
fixShape (S.In xs) = "(" ++ concatMap fixShape xs ++ ")"

finiteNest :: Int -> Int -> S.Nest []
finiteNest depth branching = S.Nest
    (if depth <= 0 then Nothing else Just (replicate branching (finiteNest (depth - 1) branching)))

nestShape :: S.Nest [] -> String
nestShape (S.Nest Nothing) = "Nothing"
nestShape (S.Nest (Just xs)) = "Just[" ++ concatMap nestShape xs ++ "]"

mixedInput :: Sample -> S.Mixed Maybe Int
mixedInput s = case variant s `mod` 4 of
    0 -> S.Leaf (number s)
    1 -> S.Branch Nothing
    2 -> S.Branch (Just (S.Leaf [number s, other s]))
    _ -> S.Branch (Just (S.Branch (Just (S.Leaf [numbers s, [number s, other s]]))))

mixedShape :: Show a => S.Mixed Maybe a -> String
mixedShape (S.Leaf a) = "Leaf " ++ show a
mixedShape (S.Branch Nothing) = "Branch Nothing"
mixedShape (S.Branch (Just x)) = "Branch (Just " ++ mixedShape x ++ ")"

fixPlan :: D.PlanCodec (FixT ListT) (FixD ListD)
fixPlan = codec
  where
    codec = D.PlanCodec encode decode observe
    children = D.thunkPlan (D.listPlan codec)
    encode (InD d) = D.ConP "In" [D.toPlan children d]
    decode (D.ConP "In" [d]) = InD <$> D.fromPlan children d
    decode _ = Nothing
    observe (InT a) (InD d) = D.ConP "In" [D.viewPlan children a d]

nestPlan :: D.PlanCodec (NestT ListT) (NestD ListD)
nestPlan = codec
  where
    codec = D.PlanCodec encode decode observe
    optional = D.thunkPlan (D.maybePlan (D.listPlan codec))
    encode (NestD d) = D.ConP "Nest" [D.toPlan optional d]
    decode (D.ConP "Nest" [d]) = NestD <$> D.fromPlan optional d
    decode _ = Nothing
    observe (NestT a) (NestD d) = D.ConP "Nest" [D.viewPlan optional a d]

-- The generated HigherD tracks constructor arguments at several kinds. Let
-- inference connect its field to the concrete inner codec in each sandbox.
higherPlan inner = D.PlanCodec encode decode observe
  where
    field = D.thunkPlan inner
    encode (HigherD d) = D.ConP "Higher" [D.toPlan field d]
    decode (D.ConP "Higher" [d]) = HigherD <$> D.fromPlan field d
    decode _ = Nothing
    observe (HigherT a) (HigherD d) = D.ConP "Higher" [D.viewPlan field a d]

layerPlan :: D.PlanCodec (LayerT Int) (M.Dem (LayerT Int))
layerPlan = D.PlanCodec encode decode observe
  where
    field = D.thunkPlan D.intPlan
    encode (LayerD d) = D.ConP "Layer" [D.toPlan field d]
    decode (D.ConP "Layer" [d]) = LayerD <$> D.fromPlan field d
    decode _ = Nothing
    observe (LayerT a) (LayerD d) = D.ConP "Layer" [D.viewPlan field a d]

taggedPlan :: D.PlanCodec (TaggedT 'S.Key) (M.Dem (TaggedT 'S.Key))
taggedPlan = D.PlanCodec encode decode observe
  where
    field = D.thunkPlan D.intPlan
    encode (TaggedD d) = D.ConP "Tagged" [D.toPlan field d]
    decode (D.ConP "Tagged" [d]) = TaggedD <$> D.fromPlan field d
    decode _ = Nothing
    observe (TaggedT a) (TaggedD d) = D.ConP "Tagged" [D.viewPlan field a d]

mixedPlan :: D.PlanCodec a d -> D.PlanCodec (MixedT M.MaybeT a) (MixedD M.MaybeD d)
mixedPlan element = D.PlanCodec encode decode observe
  where
    leaf = D.thunkPlan element
    branch = D.thunkPlan (D.maybePlan (mixedPlan (D.listPlan element)))
    encode (LeafD d) = D.ConP "Leaf" [D.toPlan leaf d]
    encode (BranchD d) = D.ConP "Branch" [D.toPlan branch d]
    decode (D.ConP "Leaf" [d]) = LeafD <$> D.fromPlan leaf d
    decode (D.ConP "Branch" [d]) = BranchD <$> D.fromPlan branch d
    decode _ = Nothing
    observe (LeafT a) (LeafD d) = D.ConP "Leaf" [D.viewPlan leaf a d]
    observe (BranchT a) (BranchD d) = D.ConP "Branch" [D.viewPlan branch a d]
    observe (LeafT _) _ = D.ConP "Leaf" []
    observe (BranchT _) _ = D.ConP "Branch" []

higherBudget :: D.Plan -> Int
higherBudget plan = sum [D.constructorBudget tag plan | tag <- ["Higher", "Layer", "In", "Tagged"]]

mixedBudget :: D.Plan -> Int
mixedBudget plan = D.constructorBudget "Leaf" plan + D.constructorBudget "Branch" plan
