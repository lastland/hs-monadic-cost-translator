{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances,
    UndecidableSuperClasses #-}
module UtilitySiblings where
import RenamedUtility (Permit)
import qualified Memorist.Memorist as M

class Permit f a => Sibling f a where
    sibling :: f a -> M.Dem (f a)
instance Permit f a => Sibling f a where
    sibling = M.fullDemand

-- A legitimate superclass selection does not confer permission on sibling.
data Wrap f a = Wrap (f a)
instance Sibling f a => M.Demand (Wrap f a) where
    type Dem (Wrap f a) = M.Dem (f a)
    fullDemand (Wrap x) = M.fullDemand x
    usage (Wrap x) dx = M.usage x dx

-- A local callable helper remains independent, even inside an actual method.
data Local f a = Local (f a)
instance Permit f a => M.Demand (Local f a) where
    type Dem (Local f a) = M.Dem (f a)
    fullDemand (Local x) = helper x
      where
        helper y = M.fullDemand y
        unused y = M.fullDemand y

-- A source-only discarded sibling is still checked.
data Dead f a = Dead (f a)
instance Permit f a => M.Demand (Dead f a) where
    type Dem (Dead f a) = M.Dem (f a)
    fullDemand (Dead x) = fst (M.fullDemand x, helper x)
      where helper y = M.fullDemand y

-- The other field of the very same dictionary is not a utility method.
data Calls f a = Calls (f a)
instance Sibling f a => M.Demand (Calls f a) where
    type Dem (Calls f a) = M.Dem (f a)
    fullDemand (Calls x) = sibling x
