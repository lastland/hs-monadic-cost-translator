{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, TypeFamilies,
    UndecidableInstances #-}
-- Independent small semantics, specialized only at the observed Maybe
-- representation. No ShapeK/ReprK vocabulary or generated support is copied.
module Generated (HigherT(..), HigherD(..), wrapM, firstM, observedM) where
import qualified Example as S
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L

data HigherT f a = HigherT (M.Th a) (M.Th (f a))
data HigherD f a = HigherD (M.ThD a) (M.ThD (f a))
deriving instance (Eq a, Eq (f a)) => Eq (HigherD f a)
deriving instance (Show a, Show (f a)) => Show (HigherD f a)

instance M.Repr a => M.Repr (S.Higher Maybe a) where
    type Rep (S.Higher Maybe a) = HigherT M.MaybeT (M.Rep a)
    reprM (S.Higher scalar applied) = do
        first <- M.lazy (M.reprM scalar)
        second <- M.lazy (M.reprM applied)
        pure (HigherT first second)
    abstract (HigherT scalar applied) =
        S.Higher (M.abstract (M.tval scalar)) (M.abstract (M.tval applied))

instance M.Demand a => M.Demand (HigherT M.MaybeT a) where
    type Dem (HigherT M.MaybeT a) = HigherD M.MaybeD (M.Dem a)
    fullDemand (HigherT scalar applied) =
        HigherD (M.fullDemand scalar) (M.fullDemand applied)
    usage (HigherT scalar applied) (HigherD ds da) =
        M.usage scalar ds `M.union` M.usage applied da

wrapM :: M.Th a -> M.Th (f a) -> M.MemoristM (HigherT f a)
wrapM scalar applied = M.tick >> pure (HigherT scalar applied)
firstM :: M.Th (HigherT L.ListT a) -> M.MemoristM a
firstM input = do
    M.tick
    HigherT _ applied <- M.force input
    list <- M.force applied
    case list of
        L.ConsT first _ -> M.force first
        L.NilT -> error "empty input"

-- The source enters observed, wrap and first once. Its scalar field and
-- list tail are unused; the only demanded value is the original argument.
observedM :: M.Th a -> M.MemoristM a
observedM input = M.tick >> M.tick >> M.tick >> M.force input
