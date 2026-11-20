{-# LANGUAGE TypeFamilies #-}
module Generated (TreeT(..), TreeD(..)) where

import qualified Example as Source
import qualified Memorist.Memorist as M

data TreeT a = LeafT | NodeT (M.Th a) (M.Th (TreeT a)) (M.Th (TreeT a))
data TreeD a = LeafD | NodeD (M.ThD a) (M.ThD (TreeD a)) (M.ThD (TreeD a))

-- A node suspends its payload and each child separately. Conversion does not
-- inspect any of them until the observer requests that field.
instance M.Repr a => M.Repr (Source.Tree a) where
    type Rep (Source.Tree a) = TreeT (M.Rep a)
    reprM Source.Leaf = pure LeafT
    reprM (Source.Node value left right) = do
        payload <- M.lazy (M.reprM value)
        first <- M.lazy (M.reprM left)
        second <- M.lazy (M.reprM right)
        pure (NodeT payload first second)
    abstract LeafT = Source.Leaf
    abstract (NodeT value left right) =
        Source.Node (M.abstract (M.tval value)) (M.abstract (M.tval left)) (M.abstract (M.tval right))

instance M.Demand a => M.Demand (TreeT a) where
    type Dem (TreeT a) = TreeD (M.Dem a)
    fullDemand LeafT = LeafD
    fullDemand (NodeT value left right) = NodeD (M.fullDemand value) (M.fullDemand left) (M.fullDemand right)
    usage (NodeT value left right) (NodeD payload first second) =
        M.usage value payload `M.union` M.usage left first `M.union` M.usage right second
    usage _ _ = M.empty
