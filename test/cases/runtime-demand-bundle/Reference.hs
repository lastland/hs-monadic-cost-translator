{-# LANGUAGE TypeFamilies #-}
module Generated where

import qualified Example as Source
import qualified Memorist.Data.List as L
import Memorist.Memorist (Demand(..), Repr(..))
import qualified Memorist.Memorist as M

-- Each source field is suspended independently. The scalar argument itself
-- stays shared by the three occurrences of x in the source equation.
data BundleT = BundleT
    (M.Th (L.ListT Int))
    (M.Th (M.PairT Int Bool))
    (M.Th (M.MaybeT (L.ListT Int)))

data BundleD = BundleD
    (M.ThD (L.ListD Int))
    (M.ThD (M.PairD Int Bool))
    (M.ThD (M.MaybeD (L.ListD Int)))
    deriving (Eq, Show)

instance M.Repr Source.Bundle where
    type Rep Source.Bundle = BundleT
    reprM (Source.Bundle xs pair rest) =
        BundleT <$> M.lazy (M.reprM xs) <*> M.lazy (M.reprM pair) <*> M.lazy (M.reprM rest)
    abstract (BundleT xs pair rest) =
        Source.Bundle (M.abstract (M.tval xs)) (M.abstract (M.tval pair)) (M.abstract (M.tval rest))

instance M.Demand BundleT where
    type Dem BundleT = BundleD
    fullDemand (BundleT xs pair rest) =
        BundleD (M.fullDemand xs) (M.fullDemand pair) (M.fullDemand rest)
    usage (BundleT xs pair rest) (BundleD dx dp dr) =
        M.usage xs dx `M.union` M.usage pair dp `M.union` M.usage rest dr

singleton :: M.Th Int -> M.MemoristM (L.ListT Int)
singleton x = do
    end <- M.lazy (pure L.NilT)
    pure (L.ConsT x end)

bundleM :: M.Th Int -> M.Th Bool -> M.MemoristM BundleT
bundleM x flag = do
    M.tick
    xs <- M.lazy (singleton x)
    pair <- M.lazy (pure (M.PairT x flag))
    rest <- M.lazy $ do
        inner <- M.lazy (singleton x)
        pure (M.JustT inner)
    pure (BundleT xs pair rest)

partialM :: M.Th Int -> M.MemoristM BundleT
partialM x = do
    M.tick
    xs <- M.lazy (error "unused list")
    pair <- M.lazy $ do
        flag <- M.lazy (error "unused boolean")
        pure (M.PairT x flag)
    rest <- M.lazy (error "unused optional list")
    pure (BundleT xs pair rest)

firstM :: M.Th BundleT -> M.MemoristM Int
firstM input = do
    M.tick
    BundleT _ pair _ <- M.force input
    M.PairT x _ <- M.force pair
    M.force x

secondM :: M.Th BundleT -> M.MemoristM Bool
secondM input = do
    M.tick
    BundleT _ pair _ <- M.force input
    M.PairT _ flag <- M.force pair
    M.force flag

ignoreM :: M.Th BundleT -> M.MemoristM Int
ignoreM _ = M.tick >> pure 0

twiceM :: M.Th BundleT -> M.MemoristM (M.PairT Int Int)
twiceM input = do
    M.tick
    left <- M.lazy (firstM input)
    right <- M.lazy (firstM input)
    pure (M.PairT left right)

itemsM :: M.Th BundleT -> M.MemoristM (L.ListT Int)
itemsM input = do
    M.tick
    BundleT xs _ _ <- M.force input
    M.force xs

choiceM :: M.Th BundleT -> M.MemoristM (M.PairT Int Bool)
choiceM input = do
    M.tick
    BundleT _ pair _ <- M.force input
    M.force pair

nestedM :: M.Th BundleT -> M.MemoristM (M.MaybeT (L.ListT Int))
nestedM input = do
    M.tick
    BundleT _ _ rest <- M.force input
    M.force rest
