module Case (probes) where

import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import qualified DemandSupport as D
import ReferenceSupport

probes :: [Probe]
probes =
    [ demanded "grow-finite-tree" (\s -> treeView s (width s) []) (\s -> treeDemand s (width s))
        (\s -> M.fromPure1 growM (number s))
    , demanded "grow-root-value" (\_ -> rootValue) (\_ -> rootDemand) (\s -> M.fromPure1 growM (other s))
    , demanded "grow-custom-outer-demand" (\_ -> outerTree) (\_ -> outerTreeDemand)
        (\s -> M.fromPure1 growM (number s))
    , demanded "grow-skipped-bottom-field" (\_ -> outerTree) (\_ -> outerTreeDemand) $ \_ -> do
        x <- M.lazy (pure (error "skipped grow field" :: Int))
        growM x
    , failure "grow-demanded-bottom-field" $ \_ -> do
        x <- M.lazy (error "demanded grow field")
        tree <- growM x
        case tree of
            LeafT -> pure 0
            NodeT a _ _ -> M.force a
    , demanded "nested-conversion" (\_ -> nestedView) (\_ -> M.fullDemand)
        (\s -> M.reprM (nestedSample s (width s)) :: M.MemoristM (NestedT Int))
    , demanded "nested-outer-only" (\_ -> outerNested) (\_ -> outerDemand)
        (\s -> M.reprM (nestedSample s (variant s `mod` 4)) :: M.MemoristM (NestedT Int))
    , generatedDemands $ D.demandProbe "generated-tree-demands"
        (\s -> M.fromPure1 growM (number s)) genTreeDemand shrinkTreeDemand
        (D.toDemandCodec (treePlan D.intPlan) D.treeBudget)
    , generatedDemands $ D.demandProbe "generated-nested-demands"
        (\s -> M.reprM (nestedSample s (width s)) :: M.MemoristM (NestedT Int))
        genNestedDemand shrinkNestedDemand
        (D.toDemandCodec (nestedPlan D.intPlan) nestedBudget)
    ]
  where
    rootValue LeafT = Nothing
    rootValue (NodeT a _ _) = Just (M.tval a)
    rootDemand LeafT = LeafD
    rootDemand (NodeT a _ _) = NodeD (M.Need (M.tval a)) M.Skip M.Skip
    outerTree LeafT = "Leaf"
    outerTree (NodeT _ _ _) = "Node"
    outerTreeDemand LeafT = LeafD
    outerTreeDemand (NodeT _ _ _) = NodeD M.Skip M.Skip M.Skip
    outerNested (EndT _) = "End"
    outerNested (MoreT _) = "More"
    outerDemand (EndT _) = EndD M.Skip
    outerDemand (MoreT _) = MoreD M.Skip

readField :: Sample -> Int -> Bool
readField s depth = if even (depth + variant s) then leftDemand s else rightDemand s

treeView :: Sample -> Int -> [Bool] -> TreeT Int -> [([Bool], Bool, Maybe Int)]
treeView _ _ path LeafT = [(path, False, Nothing)]
treeView s depth path (NodeT a l r) =
    (path, True, if readField s depth then Just (M.tval a) else Nothing) :
    (if depth > 0 && leftDemand s then treeView s (depth - 1) (path ++ [False]) (M.tval l) else []) ++
    (if depth > 0 && rightDemand s then treeView s (depth - 1) (path ++ [True]) (M.tval r) else [])

treeDemand :: Sample -> Int -> TreeT Int -> TreeD Int
treeDemand _ _ LeafT = LeafD
treeDemand s depth (NodeT a l r) = NodeD
    (if readField s depth then M.Need (M.tval a) else M.Skip)
    (if depth > 0 && leftDemand s then M.Need (treeDemand s (depth - 1) (M.tval l)) else M.Skip)
    (if depth > 0 && rightDemand s then M.Need (treeDemand s (depth - 1) (M.tval r)) else M.Skip)

-- A finite non-regular input changes the element type at each More node.
nestedInput :: Int -> a -> S.Nested a
nestedInput depth a
    | depth <= 0 = S.End a
    | otherwise = S.More (nestedInput (depth - 1) [a, a])

nestedSample :: Sample -> Int -> S.Nested Int
nestedSample s depth
    | depth <= 0 = S.End (number s)
    | otherwise = S.More (nestedInput (depth - 1) [number s, other s])

nestedView :: NestedT Int -> String
nestedView x = view (M.abstract x :: S.Nested Int)
  where
    view :: Show a => S.Nested a -> String
    view (S.End a) = "End " ++ show a
    view (S.More rest) = "More (" ++ view rest ++ ")"

treePlan :: D.PlanCodec a d -> D.PlanCodec (TreeT a) (TreeD d)
treePlan element = codec
  where
    codec = D.PlanCodec encode decode observe
    field = D.thunkPlan element
    child = D.thunkPlan codec
    encode LeafD = D.ConP "Leaf" []
    encode (NodeD a l r) = D.ConP "Node" [D.toPlan field a, D.toPlan child l, D.toPlan child r]
    decode (D.ConP "Leaf" []) = Just LeafD
    decode (D.ConP "Node" [a, l, r]) = NodeD <$> D.fromPlan field a <*> D.fromPlan child l <*> D.fromPlan child r
    decode _ = Nothing
    observe LeafT _ = D.ConP "Leaf" []
    observe (NodeT a l r) (NodeD da dl dr) = D.ConP "Node"
        [D.viewPlan field a da, D.viewPlan child l dl, D.viewPlan child r dr]
    observe (NodeT _ _ _) _ = D.ConP "Node" []

nestedPlan :: D.PlanCodec a d -> D.PlanCodec (NestedT a) (NestedD d)
nestedPlan element = D.PlanCodec encode decode observe
  where
    end = D.thunkPlan element
    more = D.thunkPlan (nestedPlan (D.listPlan element))
    encode (EndD d) = D.ConP "End" [D.toPlan end d]
    encode (MoreD d) = D.ConP "More" [D.toPlan more d]
    decode (D.ConP "End" [d]) = EndD <$> D.fromPlan end d
    decode (D.ConP "More" [d]) = MoreD <$> D.fromPlan more d
    decode _ = Nothing
    observe (EndT a) (EndD d) = D.ConP "End" [D.viewPlan end a d]
    observe (MoreT a) (MoreD d) = D.ConP "More" [D.viewPlan more a d]
    observe (EndT _) _ = D.ConP "End" []
    observe (MoreT _) _ = D.ConP "More" []

nestedBudget :: D.Plan -> Int
nestedBudget plan = D.constructorBudget "End" plan + D.constructorBudget "More" plan
