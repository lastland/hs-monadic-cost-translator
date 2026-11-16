{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Generated
    ( TreeT(..), TreeD(..), NestedT(..), NestedD(..), growM
    , genTreeDemand, shrinkTreeDemand, genNestedDemand, shrinkNestedDemand
    ) where

import qualified Example as S
import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT, ListD)
import Test.QuickCheck (Gen, chooseInt)

-- These small representation and demand types are handwritten, not generated.
data TreeT a = LeafT | NodeT (M.Th a) (M.Th (TreeT a)) (M.Th (TreeT a))
data TreeD a = LeafD | NodeD (M.ThD a) (M.ThD (TreeD a)) (M.ThD (TreeD a))

instance M.Repr a => M.Repr (S.Tree a) where
    type Rep (S.Tree a) = TreeT (M.Rep a)
    reprM S.Leaf = pure LeafT
    reprM (S.Node a l r) = NodeT
        <$> M.lazy (M.reprM a) <*> M.lazy (M.reprM l) <*> M.lazy (M.reprM r)
    abstract LeafT = S.Leaf
    abstract (NodeT a l r) = S.Node
        (M.abstract (M.tval a)) (M.abstract (M.tval l)) (M.abstract (M.tval r))

instance M.Demand a => M.Demand (TreeT a) where
    type Dem (TreeT a) = TreeD (M.Dem a)
    fullDemand LeafT = LeafD
    fullDemand (NodeT a l r) = NodeD (M.fullDemand a) (M.fullDemand l) (M.fullDemand r)
    usage (NodeT a l r) (NodeD da dl dr) =
        M.usage a da `M.union` M.usage l dl `M.union` M.usage r dr
    usage _ _ = M.empty

data NestedT a = EndT (M.Th a) | MoreT (M.Th (NestedT (ListT a)))
data NestedD a = EndD (M.ThD a) | MoreD (M.ThD (NestedD (ListD a)))

instance M.Repr a => M.Repr (S.Nested a) where
    type Rep (S.Nested a) = NestedT (M.Rep a)
    reprM (S.End a) = EndT <$> M.lazy (M.reprM a)
    reprM (S.More rest) = MoreT <$> M.lazy (M.reprM rest)
    abstract (EndT a) = S.End (M.abstract (M.tval a))
    abstract (MoreT rest) = S.More (M.abstract (M.tval rest))

instance M.Demand a => M.Demand (NestedT a) where
    type Dem (NestedT a) = NestedD (M.Dem a)
    fullDemand (EndT a) = EndD (M.fullDemand a)
    fullDemand (MoreT rest) = MoreD (M.fullDemand rest)
    usage (EndT a) (EndD da) = M.usage a da
    usage (MoreT rest) (MoreD drest) = M.usage rest drest
    usage _ _ = M.empty

growM :: M.Th a -> M.MemoristM (TreeT a)
growM x = do
    M.tick
    -- x is shared by every node; the two recursive calls are distinct thunks.
    l <- M.lazy (growM x)
    r <- M.lazy (growM x)
    pure (NodeT x l r)

-- These independent structural helpers make the same static plan catalog
-- typecheck on either side. Only the generated side chooses replay plans;
-- the observer must not compare the two generators' random choices.
genTreeDemand :: M.Demand a => Int -> TreeT a -> Gen (TreeD (M.Dem a))
genTreeDemand _ LeafT = pure LeafD
genTreeDemand budget (NodeT a l r) = do
    scalarBudget <- chooseInt (0, min 1 (max 0 budget))
    leftBudget <- chooseInt (0, max 0 budget - scalarBudget)
    let rightBudget = max 0 budget - scalarBudget - leftBudget
    dl <- branch leftBudget l
    dr <- branch rightBudget r
    pure (NodeD (if scalarBudget == 0 then M.Skip else M.Need (M.fullDemand (M.tval a))) dl dr)
  where
    branch allowance child
        | allowance < 2 = pure M.Skip
        | otherwise = M.Need <$> genTreeDemand (allowance - 2) (M.tval child)

shrinkTreeDemand :: TreeT a -> TreeD d -> [TreeD d]
shrinkTreeDemand (NodeT _ l r) (NodeD a dl dr) =
    [NodeD M.Skip dl dr | M.Need _ <- [a]] ++
    [NodeD a M.Skip dr | M.Need _ <- [dl]] ++
    [NodeD a dl M.Skip | M.Need _ <- [dr]] ++
    [NodeD a (M.Need smaller) dr | M.Need d <- [dl], smaller <- shrinkTreeDemand (M.tval l) d] ++
    [NodeD a dl (M.Need smaller) | M.Need d <- [dr], smaller <- shrinkTreeDemand (M.tval r) d]
shrinkTreeDemand _ _ = []

genNestedDemand :: M.Demand a => Int -> NestedT a -> Gen (NestedD (M.Dem a))
genNestedDemand budget value = do
    enter <- chooseInt (0, max 0 (min 1 budget))
    case value of
        EndT a -> pure (EndD (if enter == 0 then M.Skip else M.Need (M.fullDemand (M.tval a))))
        MoreT rest -> if enter == 0 then pure (MoreD M.Skip)
            else MoreD . M.Need <$> genNestedDemand (budget - 1) (M.tval rest)

shrinkNestedDemand :: NestedT a -> NestedD d -> [NestedD d]
shrinkNestedDemand (EndT _) (EndD (M.Need _)) = [EndD M.Skip]
shrinkNestedDemand (MoreT rest) (MoreD (M.Need d)) =
    MoreD M.Skip : [MoreD (M.Need smaller) | smaller <- shrinkNestedDemand (M.tval rest) d]
shrinkNestedDemand _ _ = []
