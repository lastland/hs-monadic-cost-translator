{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures #-}
{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Generated where

import qualified Example as S
import Data.Kind (Type)
import GHC.TypeLits (Nat, Symbol)
import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..), ListD(..))
import qualified Test.QuickCheck as QC

-- Concrete conversion instances suffice for the exercised higher-kinded
-- applications. There is deliberately no generated dictionary machinery here.
data HigherT (f :: k -> Type) (a :: k) = HigherT (M.Th (f a))
data HigherD a = HigherD (M.ThD a)
instance M.Demand (f a) => M.Demand (HigherT f a) where
    type Dem (HigherT f a) = HigherD (M.Dem (f a))
    fullDemand (HigherT x) = HigherD (M.fullDemand x)
    usage (HigherT x) (HigherD d) = M.usage x d

data LayerT a = LayerT (M.Th a)
data LayerD a = LayerD (M.ThD a)
instance M.Repr a => M.Repr (S.Layer a) where
    type Rep (S.Layer a) = LayerT (M.Rep a)
    reprM (S.Layer a) = LayerT <$> M.lazy (M.reprM a)
    abstract (LayerT a) = S.Layer (M.abstract (M.tval a))
instance M.Demand a => M.Demand (LayerT a) where
    type Dem (LayerT a) = LayerD (M.Dem a)
    fullDemand (LayerT a) = LayerD (M.fullDemand a)
    usage (LayerT a) (LayerD d) = M.usage a d
instance M.Demand a => M.LBoundDemand (LayerT a) where
    minDemand _ = LayerD M.Skip

instance M.Repr a => M.Repr (S.Higher S.Layer a) where
    type Rep (S.Higher S.Layer a) = HigherT LayerT (M.Rep a)
    reprM (S.Higher a) = HigherT <$> M.lazy (M.reprM a)
    abstract (HigherT a) = S.Higher (M.abstract (M.tval a))
instance M.Repr a => M.Repr (S.Higher [] a) where
    type Rep (S.Higher [] a) = HigherT ListT (M.Rep a)
    reprM (S.Higher a) = HigherT <$> M.lazy (M.reprM a)
    abstract (HigherT a) = S.Higher (M.abstract (M.tval a))
instance M.Repr a => M.Repr (S.Higher Maybe a) where
    type Rep (S.Higher Maybe a) = HigherT M.MaybeT (M.Rep a)
    reprM (S.Higher a) = HigherT <$> M.lazy (M.reprM a)
    abstract (HigherT a) = S.Higher (M.abstract (M.tval a))

data FixT (f :: Type -> Type) = InT (M.Th (f (FixT f)))
data FixD (f :: Type -> Type) = InD (M.ThD (f (FixD f)))
instance M.Repr (S.Fix []) where
    type Rep (S.Fix []) = FixT ListT
    reprM (S.In a) = InT <$> M.lazy (M.reprM a)
    abstract (InT a) = S.In (M.abstract (M.tval a))
instance M.Demand (FixT ListT) where
    type Dem (FixT ListT) = FixD ListD
    fullDemand (InT a) = InD (M.fullDemand a)
    usage (InT a) (InD d) = M.usage a d
instance M.LBoundDemand (FixT ListT) where
    minDemand _ = InD M.Skip
instance M.Repr (S.Higher S.Fix []) where
    type Rep (S.Higher S.Fix []) = HigherT FixT ListT
    reprM (S.Higher a) = HigherT <$> M.lazy (M.reprM a)
    abstract (HigherT a) = S.Higher (M.abstract (M.tval a))

data NestT (f :: Type -> Type) = NestT (M.Th (M.MaybeT (f (NestT f))))
data NestD (f :: Type -> Type) = NestD (M.ThD (M.MaybeD (f (NestD f))))
instance M.Repr (S.Nest []) where
    type Rep (S.Nest []) = NestT ListT
    reprM (S.Nest a) = NestT <$> M.lazy (M.reprM a)
    abstract (NestT a) = S.Nest (M.abstract (M.tval a))
instance M.Demand (NestT ListT) where
    type Dem (NestT ListT) = NestD ListD
    fullDemand (NestT a) = NestD (M.fullDemand a)
    usage (NestT a) (NestD d) = M.usage a d

data MixedT (f :: Type -> Type) a = LeafT (M.Th a) | BranchT (M.Th (f (MixedT f (ListT a))))
data MixedD (f :: Type -> Type) a = LeafD (M.ThD a) | BranchD (M.ThD (f (MixedD f (ListD a))))
instance M.Repr a => M.Repr (S.Mixed Maybe a) where
    type Rep (S.Mixed Maybe a) = MixedT M.MaybeT (M.Rep a)
    reprM (S.Leaf a) = LeafT <$> M.lazy (M.reprM a)
    reprM (S.Branch a) = BranchT <$> M.lazy (M.reprM a)
    abstract (LeafT a) = S.Leaf (M.abstract (M.tval a))
    abstract (BranchT a) = S.Branch (M.abstract (M.tval a))
instance M.Demand a => M.Demand (MixedT M.MaybeT a) where
    type Dem (MixedT M.MaybeT a) = MixedD M.MaybeD (M.Dem a)
    fullDemand (LeafT a) = LeafD (M.fullDemand a)
    fullDemand (BranchT a) = BranchD (M.fullDemand a)
    usage (LeafT a) (LeafD d) = M.usage a d
    usage (BranchT a) (BranchD d) = M.usage a d
    usage _ _ = M.empty

data TaggedT (i :: S.Index) = TaggedT (M.Th Int)
data TaggedD = TaggedD (M.ThD Int)
instance M.Repr (S.Tagged i) where
    type Rep (S.Tagged i) = TaggedT i
    reprM (S.Tagged n) = TaggedT <$> M.lazy (pure n)
    abstract (TaggedT n) = S.Tagged (M.tval n)
instance M.Demand (TaggedT i) where
    type Dem (TaggedT i) = TaggedD
    fullDemand (TaggedT n) = TaggedD (M.fullDemand n)
    usage (TaggedT n) (TaggedD d) = M.usage n d
instance M.LBoundDemand (TaggedT i) where
    minDemand _ = TaggedD M.Skip
instance M.Repr (S.Higher S.Tagged i) where
    type Rep (S.Higher S.Tagged i) = HigherT TaggedT i
    reprM (S.Higher a) = HigherT <$> M.lazy (M.reprM a)
    abstract (HigherT a) = S.Higher (M.abstract (M.tval a))

data TagT (i :: k) = TagT (M.Th Int)
data NumericT (f :: Nat -> Type) = NumericT (M.Th (f 0))
data NamedT (f :: Symbol -> Type) = NamedT (M.Th (f "key"))
data CharacterT (f :: Char -> Type) = CharacterT (M.Th (f 'x'))
data SelectedT (f :: S.Index -> Type) = SelectedT (M.Th (f 'S.Key))
data OperatedT (f :: S.Index -> Type) = OperatedT (M.Th (f ('S.Key 'S.:#: 'S.Key)))

readNumericM :: M.Th (NumericT TagT) -> M.MemoristM Int
readNumericM x = do
    M.tick
    NumericT tag <- M.force x
    TagT n <- M.force tag
    M.force n
readNamedM :: M.Th (NamedT TagT) -> M.MemoristM Int
readNamedM x = do
    M.tick
    NamedT tag <- M.force x
    TagT n <- M.force tag
    M.force n
readCharacterM :: M.Th (CharacterT TagT) -> M.MemoristM Int
readCharacterM x = do
    M.tick
    CharacterT tag <- M.force x
    TagT n <- M.force tag
    M.force n
readSelectedM :: M.Th (SelectedT TagT) -> M.MemoristM Int
readSelectedM x = do
    M.tick
    SelectedT tag <- M.force x
    TagT n <- M.force tag
    M.force n
readOperatedM :: M.Th (OperatedT TagT) -> M.MemoristM Int
readOperatedM x = do
    M.tick
    OperatedT tag <- M.force x
    TagT n <- M.force tag
    M.force n

indexKindsM :: M.Th Int -> M.MemoristM Int
indexKindsM x = do
    M.tick
    numericTag <- M.lazy (pure (TagT x))
    numeric <- M.lazy (pure (NumericT numericTag))
    a <- readNumericM numeric
    namedTag <- M.lazy (pure (TagT x))
    named <- M.lazy (pure (NamedT namedTag))
    b <- readNamedM named
    characterTag <- M.lazy (pure (TagT x))
    character <- M.lazy (pure (CharacterT characterTag))
    c <- readCharacterM character
    selectedTag <- M.lazy (pure (TagT x))
    selected <- M.lazy (pure (SelectedT selectedTag))
    d <- readSelectedM selected
    operatedTag <- M.lazy (pure (TagT x))
    operated <- M.lazy (pure (OperatedT operatedTag))
    e <- readOperatedM operated
    pure (a + b + c + d + e)

forestM :: M.Th Int -> M.MemoristM (FixT ListT)
forestM n = do
    M.tick
    l <- M.lazy $ do
        next <- M.lazy $ do { x <- M.force n; pure (x + 1) }
        forestM next
    r <- M.lazy $ do
        next <- M.lazy $ do { x <- M.force n; pure (x + 2) }
        forestM next
    end <- M.lazy (pure NilT)
    tailList <- M.lazy (pure (ConsT r end))
    children <- M.lazy (pure (ConsT l tailList))
    pure (InT children)

nestedM :: M.Th Int -> M.MemoristM (NestT ListT)
nestedM n = do
    M.tick
    child <- M.lazy $ do
        next <- M.lazy $ do { x <- M.force n; pure (x + 1) }
        nestedM next
    end <- M.lazy (pure NilT)
    children <- M.lazy (pure (ConsT child end))
    optional <- M.lazy (pure (M.JustT children))
    pure (NestT optional)

taggedM :: M.Th Int -> M.MemoristM (HigherT TaggedT i)
taggedM x = do
    M.tick
    tag <- M.lazy (pure (TaggedT x))
    pure (HigherT tag)

-- Independent, budgeted structural support. The plan runner uses these only
-- to typecheck the reference catalog: all replay plans come from the actual
-- generated helpers, so their random algorithms are not compared.
genHigherDemand :: M.LBoundDemand (f a) => Int -> HigherT f a -> QC.Gen (HigherD (M.Dem (f a)))
genHigherDemand budget (HigherT x) = do
    enter <- QC.chooseInt (0, min 1 (max 0 budget))
    pure (HigherD (if enter == 0 then M.Skip else M.Need (M.minDemand (M.tval x))))

shrinkHigherDemand :: HigherT f a -> HigherD d -> [HigherD d]
shrinkHigherDemand _ (HigherD (M.Need _)) = [HigherD M.Skip]
shrinkHigherDemand _ _ = []

genFixDemand :: Int -> FixT ListT -> QC.Gen (FixD ListD)
genFixDemand budget (InT xs) = do
    enter <- QC.chooseInt (0, min 1 (max 0 budget))
    if enter == 0 then pure (InD M.Skip)
        else InD . M.Need <$> genListDemand (budget - 1) genFixDemand (M.tval xs)

shrinkFixDemand :: FixT ListT -> FixD ListD -> [FixD ListD]
shrinkFixDemand (InT xs) (InD (M.Need d)) = InD M.Skip :
    [InD (M.Need smaller) | smaller <- shrinkListDemand shrinkFixDemand (M.tval xs) d]
shrinkFixDemand _ _ = []

genNestDemand :: Int -> NestT ListT -> QC.Gen (NestD ListD)
genNestDemand budget (NestT optional) = do
    enter <- QC.chooseInt (0, min 1 (max 0 budget))
    if enter == 0 then pure (NestD M.Skip) else case M.tval optional of
        M.NothingT -> pure (NestD (M.Need M.NothingD))
        M.JustT xs -> do
            ds <- genListDemand (budget - 1) genNestDemand (M.tval xs)
            pure (NestD (M.Need (M.JustD (M.Need ds))))

shrinkNestDemand :: NestT ListT -> NestD ListD -> [NestD ListD]
shrinkNestDemand (NestT optional) (NestD (M.Need (M.JustD (M.Need ds)))) =
    [NestD M.Skip, NestD (M.Need (M.JustD M.Skip))] ++ case M.tval optional of
        M.JustT xs -> [NestD (M.Need (M.JustD (M.Need smaller))) |
            smaller <- shrinkListDemand shrinkNestDemand (M.tval xs) ds]
        M.NothingT -> []
shrinkNestDemand _ (NestD (M.Need _)) = [NestD M.Skip]
shrinkNestDemand _ _ = []

genMixedDemand :: M.Demand a => Int -> MixedT M.MaybeT a -> QC.Gen (MixedD M.MaybeD (M.Dem a))
genMixedDemand budget value = do
    enter <- QC.chooseInt (0, min 1 (max 0 budget))
    case value of
        LeafT a -> pure (LeafD (if enter == 0 then M.Skip else M.Need (M.fullDemand (M.tval a))))
        BranchT optional -> if enter == 0 then pure (BranchD M.Skip) else case M.tval optional of
            M.NothingT -> pure (BranchD (M.Need M.NothingD))
            M.JustT child -> if budget <= 1 then pure (BranchD (M.Need (M.JustD M.Skip))) else do
                d <- genMixedDemand (budget - 2) (M.tval child)
                pure (BranchD (M.Need (M.JustD (M.Need d))))

shrinkMixedDemand :: MixedT M.MaybeT a -> MixedD M.MaybeD d -> [MixedD M.MaybeD d]
shrinkMixedDemand (LeafT _) (LeafD (M.Need _)) = [LeafD M.Skip]
shrinkMixedDemand (BranchT optional) (BranchD (M.Need (M.JustD (M.Need d)))) =
    [BranchD M.Skip, BranchD (M.Need (M.JustD M.Skip))] ++ case M.tval optional of
        M.JustT child -> [BranchD (M.Need (M.JustD (M.Need smaller))) |
            smaller <- shrinkMixedDemand (M.tval child) d]
        M.NothingT -> []
shrinkMixedDemand (BranchT _) (BranchD (M.Need _)) = [BranchD M.Skip]
shrinkMixedDemand _ _ = []

genListDemand :: Int -> (Int -> a -> QC.Gen d) -> ListT a -> QC.Gen (ListD d)
genListDemand _ _ NilT = pure NilD
genListDemand budget element (ConsT x rest) = do
    headBudget <- QC.chooseInt (0, max 0 budget)
    let tailBudget = max 0 budget - headBudget
    dx <- if headBudget < 2 then pure M.Skip else M.Need <$> element (headBudget - 2) (M.tval x)
    ds <- if tailBudget < 2 then pure M.Skip
        else M.Need <$> genListDemand (tailBudget - 2) element (M.tval rest)
    pure (ConsD dx ds)

shrinkListDemand :: (a -> d -> [d]) -> ListT a -> ListD d -> [ListD d]
shrinkListDemand element (ConsT x rest) (ConsD dx ds) =
    [ConsD M.Skip ds | M.Need _ <- [dx]] ++
    [ConsD dx M.Skip | M.Need _ <- [ds]] ++
    [ConsD (M.Need smaller) ds | M.Need d <- [dx], smaller <- element (M.tval x) d] ++
    [ConsD dx (M.Need smaller) | M.Need d <- [ds], smaller <- shrinkListDemand element (M.tval rest) d]
shrinkListDemand _ _ _ = []

instance Show (FixD ListD) where
    showsPrec p (InD fields) = showParen (p > 10) (showString "InD " . showsPrec 11 fields)

instance QC.Arbitrary (FixD ListD) where
    arbitrary = QC.sized bare
      where
        bare budget
            | budget <= 0 = pure (InD M.Skip)
            | otherwise = do
                shape <- QC.chooseInt (0, 2)
                case shape of
                    0 -> pure (InD M.Skip)
                    1 -> pure (InD (M.Need NilD))
                    _ -> do
                        child <- bare (budget - 1)
                        pure (InD (M.Need (ConsD (M.Need child) M.Skip)))
    shrink (InD (M.Need _)) = [InD M.Skip]
    shrink _ = []

instance M.DemandArb (FixT ListT) where
    arbDemandFrom value = QC.sized (\budget -> genFixDemand budget value)
