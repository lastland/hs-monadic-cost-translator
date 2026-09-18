{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints, RankNTypes, TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
-- Constructor WHNF is observable in this laziness probe; newtype would change it.
{- HLINT ignore "Use newtype instead of data" -}
module Main (main) where

import Data.Kind (Type, Constraint)
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L
import Control.Monad (unless, forM_)
import Data.Proxy (Proxy(..))
import qualified Test.QuickCheck as QC
import qualified Memorist.Base.Thunk as MT
import System.Timeout (timeout)

data Higher f a = Higher (f a)
data Fix f = In (f (Fix f))
data HigherT f a = HigherT (M.Th (f a))
data FixT f = InT (M.Th (f (FixT f)))
data HigherD f a = HigherD (M.ThD (f a))
data FixD f = InD (M.ThD (f (FixD f)))

type RepT :: forall k. k -> k
type RepT (a :: k) = RepK k a
type RepK :: forall k -> k -> k
type family RepK k a where
    RepK Type a = M.Rep a
    RepK (Type -> Type) [] = L.ListT
    RepK (Type -> Type) Maybe = M.MaybeT
    RepK (Type -> Type -> Type) (,) = M.PairT
    RepK ((k -> Type) -> k -> Type) Higher = HigherT
    RepK ((Type -> Type) -> Type) Fix = FixT
    RepK k (f (a :: j)) = RepK (j -> k) f (RepK j a)
    RepK k a = a

type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a where
    ReprReq Type a = M.Repr a
    ReprReq (k -> l) f = ReprF f
    ReprReq k a = ()

type ReprK :: forall k. k -> Constraint
type ReprK (a :: k) = ReprReq k a

type ReprApp :: forall k l. (k -> l) -> k -> Constraint
class (ReprK (f a), RepT (f a) ~ RepT f (RepT a)) => ReprApp f a
instance (ReprK (f a), RepT (f a) ~ RepT f (RepT a)) => ReprApp f a

type ReprF :: forall k l. (k -> l) -> Constraint
class (forall a. ReprK a => ReprApp f a) => ReprF f
instance (forall a. ReprK a => ReprApp f a) => ReprF f

instance (ReprK f, ReprK a) => M.Repr (Higher f a) where
    type Rep (Higher f a) = HigherT (RepT f) (RepT a)
    reprM (Higher x) = HigherT <$> M.lazy (M.reprM x)
    abstract (HigherT x) = Higher (M.abstract (M.tval x))

instance forall f. ReprK f => M.Repr (Fix f) where
    type Rep (Fix f) = FixT (RepT f)
    reprM (In x) = withConstraint (Proxy :: Proxy (ReprApp f (Fix f))) $ InT <$> M.lazy (M.reprM x)
    abstract (InT x) = withConstraint (Proxy :: Proxy (ReprApp f (Fix f))) $ In (M.abstract (M.tval x))

type DemT :: forall k. k -> k
type DemT (a :: k) = DemK k a
type DemK :: forall k -> k -> k
type family DemK k a where
    DemK Type a = M.Dem a
    DemK (Type -> Type) L.ListT = L.ListD
    DemK (Type -> Type) M.MaybeT = M.MaybeD
    DemK (Type -> Type -> Type) M.PairT = M.PairD
    DemK ((k -> Type) -> k -> Type) HigherT = HigherD
    DemK ((Type -> Type) -> Type) FixT = FixD
    DemK k (f (a :: j)) = DemK (j -> k) f (DemK j a)
    DemK k a = a

type DemandReq :: forall k -> k -> Constraint
type family DemandReq k a where
    DemandReq Type a = M.Demand a
    DemandReq (k -> l) f = DemandF f
    DemandReq k a = ()
type DemandK :: forall k. k -> Constraint
type DemandK (a :: k) = DemandReq k a
type DemandApp :: forall k l. (k -> l) -> k -> Constraint
class (DemandK (f a), DemT (f a) ~ DemT f (DemT a)) => DemandApp f a
instance (DemandK (f a), DemT (f a) ~ DemT f (DemT a)) => DemandApp f a
type DemandF :: forall k l. (k -> l) -> Constraint
class (forall a. DemandK a => DemandApp f a) => DemandF f
instance (forall a. DemandK a => DemandApp f a) => DemandF f

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

instance (DemandK f, DemandK a) => M.Demand (HigherT f a) where
    type Dem (HigherT f a) = HigherD (DemT f) (DemT a)
    fullDemand (HigherT x) = HigherD (M.fullDemand x)
    usage (HigherT x) (HigherD d) = M.usage x d

instance forall f. DemandK f => M.Demand (FixT f) where
    type Dem (FixT f) = FixD (DemT f)
    fullDemand (InT x) = withConstraint (Proxy :: Proxy (DemandApp f (FixT f))) $ InD (M.fullDemand x)
    usage (InT x) (InD d) = withConstraint (Proxy :: Proxy (DemandApp f (FixT f))) $ M.usage x d

type ShapeReq :: (Type -> Constraint) -> forall k -> k -> Constraint
type family ShapeReq c k a where
    ShapeReq c Type a = c a
    ShapeReq c (k -> l) f = ShapeF c f
    ShapeReq c k a = ()
type ShapeK :: (Type -> Constraint) -> forall k. k -> Constraint
class ShapeReq c k a => ShapeK c (a :: k)
instance ShapeReq c k a => ShapeK c (a :: k)
type ShapeF :: (Type -> Constraint) -> forall k l. (k -> l) -> Constraint
class (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f
instance (forall a. ShapeK c a => ShapeK c (f a)) => ShapeF c f

deriving instance (ShapeK Show f, ShapeK Show a) => Show (HigherD f a)
deriving instance ShapeK Show f => Show (FixD f)
deriving instance (ShapeK Eq f, ShapeK Eq a) => Eq (HigherD f a)
deriving instance ShapeK Eq f => Eq (FixD f)

instance ShapeK QC.Arbitrary f => QC.Arbitrary (FixD f) where
    arbitrary = QC.sized $ \n -> if n <= 0 then pure (InD M.Skip)
        else InD <$> QC.liftArbitrary (QC.resize (n-1) QC.arbitrary)

class M.Demand a => GeneratedDemand a where
    genDemandBudget :: Int -> a -> QC.Gen (M.Dem a)
    shrinkDemandCompatible :: a -> M.Dem a -> [M.Dem a]

type GeneratedReq :: forall k -> k -> Constraint
type family GeneratedReq k a where
    GeneratedReq Type a = GeneratedDemand a
    GeneratedReq (k -> l) f = GeneratedF f
    GeneratedReq k a = ()
type GeneratedK :: forall k. k -> Constraint
class (DemandK a, GeneratedReq k a) => GeneratedK (a :: k)
instance (DemandK a, GeneratedReq k a) => GeneratedK (a :: k)
type GeneratedF :: forall k l. (k -> l) -> Constraint
class (forall a. GeneratedK a => GeneratedK (f a)) => GeneratedF f
instance (forall a. GeneratedK a => GeneratedK (f a)) => GeneratedF f

genField :: GeneratedDemand a => Int -> M.Th a -> QC.Gen (M.ThD (M.Dem a))
genField n t | n <= 1 = pure M.Skip
             | otherwise = QC.liftArbitrary (genDemandBudget (n-2) (M.tval t))
shrinkField :: GeneratedDemand a => M.Th a -> M.ThD (M.Dem a) -> [M.ThD (M.Dem a)]
shrinkField t = QC.liftShrink (shrinkDemandCompatible (M.tval t))

instance GeneratedDemand a => GeneratedDemand (L.ListT a) where
    genDemandBudget _ L.NilT = pure L.NilD
    genDemandBudget n (L.ConsT h t) = L.ConsD <$> genField (n `div` 2) h <*> genField (n `div` 2) t
    shrinkDemandCompatible (L.ConsT h t) (L.ConsD dh dt) =
        [L.ConsD d dt | d <- shrinkField h dh] ++ [L.ConsD dh d | d <- shrinkField t dt]
    shrinkDemandCompatible _ _ = []

instance forall f. GeneratedK f => GeneratedDemand (FixT f) where
    genDemandBudget n (InT x) = withConstraint (Proxy :: Proxy (DemandApp f (FixT f))) $
        InD <$> genField n x
    shrinkDemandCompatible (InT x) (InD d) = withConstraint (Proxy :: Proxy (DemandApp f (FixT f))) $
        [InD s | s <- shrinkField x d]

instance (GeneratedK f, ShapeK QC.Arbitrary (DemT f), ShapeK Show (DemT f)) => M.DemandArb (FixT f) where
    arbDemandFrom x = QC.sized (`genDemandBudget` x)

-- The pure input below has exactly one child at every node. These checks use
-- that source shape, not the implementation of the budget allocator.
compatible :: FixD L.ListD -> Bool
compatible (InD M.Skip) = True
compatible (InD (M.Need (L.ConsD h t))) = child h && tailDemand t
  where
    child M.Skip = True
    child (M.Need d) = compatible d
    tailDemand M.Skip = True
    tailDemand (M.Need L.NilD) = True
    tailDemand _ = False
compatible _ = False

nodes :: FixD L.ListD -> Int
nodes (InD ds) = 1 + list ds
  where
    list M.Skip = 0
    list (M.Need L.NilD) = 0
    list (M.Need (L.ConsD h t)) = child h + list t
    child M.Skip = 0
    child (M.Need d) = nodes d

main :: IO ()
main = do
    let HigherT xs = M.value (M.eval1 (M.reprM (Higher [5::Int]) :: M.MemoristM (HigherT L.ListT Int)))
    unless (M.abstract (M.tval xs) == [5::Int]) (fail "higher-kinded conversion")
    let InT xs' = M.value (M.eval1 (M.reprM (In [] :: Fix []) :: M.MemoristM (FixT L.ListT)))
    case M.tval xs' of
        L.NilT -> pure ()
        _ -> fail "recursive higher-kinded conversion"
    let emptyFix = M.value (M.eval1 (M.reprM (In [] :: Fix []) :: M.MemoristM (FixT L.ListT)))
    unless (M.fullDemand emptyFix == InD (M.Need L.NilD)) (fail "empty recursive full demand")
    let infinite = In [infinite] :: Fix []
        program = M.reprM infinite :: M.MemoristM (FixT L.ListT)
        infiniteT = M.value (M.eval1 program)
    result <- timeout 5000000 $ forM_ [0..20] $ \budget -> do
        demand <- QC.generate (QC.resize budget (M.arbDemandFrom infiniteT))
        unless (nodes demand <= budget + 1 && compatible demand &&
                all compatible (shrinkDemandCompatible infiniteT demand))
            (fail "recursive demand budget/compatibility")
        unless (MT.toInteger (M.infcost' program demand) == 0) (fail "conversion added ticks")
    case result of
        Nothing -> fail "recursive demand generation timed out"
        Just () -> putStrLn "PASS: higher-kinded conversion / finite recursive demands / compatible shrinks / zero conversion ticks"
