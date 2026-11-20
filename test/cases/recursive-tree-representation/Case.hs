module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ demanded "int-tree-selected-fields" (\s -> treeView s (width s))
        (\s -> treeDemand s (width s))
        (\s -> M.reprM (input (width s) (number s)) :: M.MemoristM (TreeT Int))
    , demanded "bool-tree-selected-fields" (\s -> treeView s 1)
        (\s -> treeDemand s 1)
        (\s -> M.reprM (Source.Node (leftDemand s) Source.Leaf
            (Source.Node (rightDemand s) Source.Leaf Source.Leaf)) :: M.MemoristM (TreeT Bool))
    , demanded "skip-bottom-payload" (\_ -> outer) (\_ -> outerDemand)
        (\_ -> M.reprM (Source.Node (error "skipped payload" :: Int) Source.Leaf Source.Leaf)
            :: M.MemoristM (TreeT Int))
    , demanded "skip-bottom-child" (\_ -> outer) (\_ -> outerDemand)
        (\_ -> M.reprM (Source.Node (1 :: Int) (error "skipped child") Source.Leaf)
            :: M.MemoristM (TreeT Int))
    , failure "demand-bottom-payload" $ \_ -> do
        tree <- M.reprM (Source.Node (error "demanded payload" :: Int) Source.Leaf Source.Leaf)
            :: M.MemoristM (TreeT Int)
        case tree of
            LeafT -> pure 0
            NodeT value _ _ -> M.force value
    , failure "demand-bottom-child" $ \_ -> do
        tree <- M.reprM (Source.Node (1 :: Int) (error "demanded child") Source.Leaf)
            :: M.MemoristM (TreeT Int)
        case tree of
            LeafT -> pure 0
            NodeT _ left _ -> do
                child <- M.force left
                case child of
                    LeafT -> pure 0
                    NodeT value _ _ -> M.force value
    ]

input :: Int -> Int -> Source.Tree Int
input depth value
    | depth <= 0 = Source.Leaf
    | otherwise = Source.Node value (input (depth - 1) (value + 1))
        (Source.Node (value - 1) Source.Leaf Source.Leaf)

treeView :: Sample -> Int -> TreeT a -> [Maybe a]
treeView _ _ LeafT = []
treeView sample depth (NodeT value left right) =
    (if even (variant sample + depth) then Just (M.tval value) else Nothing) :
    (if depth > 0 && leftDemand sample then treeView sample (depth - 1) (M.tval left) else []) ++
    (if depth > 0 && rightDemand sample then treeView sample (depth - 1) (M.tval right) else [])

treeDemand :: Sample -> Int -> TreeT a -> TreeD a
treeDemand _ _ LeafT = LeafD
treeDemand sample depth (NodeT value left right) = NodeD
    (if even (variant sample + depth) then M.Need (M.tval value) else M.Skip)
    (if depth > 0 && leftDemand sample then M.Need (treeDemand sample (depth - 1) (M.tval left)) else M.Skip)
    (if depth > 0 && rightDemand sample then M.Need (treeDemand sample (depth - 1) (M.tval right)) else M.Skip)

outer :: TreeT a -> String
outer LeafT = "Leaf"
outer NodeT{} = "Node"

outerDemand :: TreeT a -> TreeD a
outerDemand LeafT = LeafD
outerDemand NodeT{} = NodeD M.Skip M.Skip M.Skip
