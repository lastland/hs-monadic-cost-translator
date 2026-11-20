{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts,
    ScopedTypeVariables, TypeFamilies #-}
module UtilityEscapes where
import UtilityApp
import qualified Memorist.Memorist as M

openRepr :: M.Repr a => a -> M.MemoristM (M.Rep a)
openRepr = M.reprM
openAppRepr :: ReprApp f a => f a -> M.MemoristM (M.Rep (f a))
openAppRepr = M.reprM
openBox :: M.Repr a => Box a -> M.MemoristM (M.MaybeT (M.Rep a))
openBox = M.reprM
openAbstract :: ReprApp f a => M.Rep (f a) -> f a
openAbstract = M.abstract
openDemand :: DemandApp f a => f a -> M.Dem (f a)
openDemand = M.fullDemand
openUsage :: DemandApp f a => f a -> M.Dem (f a) -> M.USet
openUsage = M.usage
ordinaryObserver :: M.Th Int -> Int
ordinaryObserver = M.tval
annotatedObserver :: M.Th Int -> M.Annot
annotatedObserver = M.tannot
deadObserver :: DemandApp f a => f a -> ()
deadObserver x = let dead = M.fullDemand x in ()

-- A helper shared with an ordinary caller cannot inherit a utility role.
shared :: DemandApp f a => f a -> M.Dem (f a)
shared = M.fullDemand
ordinaryShared :: DemandApp f a => f a -> M.Dem (f a)
ordinaryShared = shared
data Share f a = Share (f a)
instance DemandApp f a => M.Demand (Share f a) where
    type Dem (Share f a) = M.Dem (f a)
    fullDemand (Share x) = shared x
