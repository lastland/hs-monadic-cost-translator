{-# OPTIONS_GHC -Wall -Werror #-}
-- | The wire format is shared by both isolated observers and the comparator.
module Protocol
    ( Sample(..), Result(..), Outcome(..), Plan(..), DemandBatch(..)
    , DemandObservation(..), Request(..), Response(..)
    ) where

-- Inputs describe finite test scenarios; they never contain bottoms or streams.
-- A case constructs those deliberately, without printing or shrinking them.
data Sample = Sample
    { number :: Int
    , other :: Int
    , numbers :: [Int]
    , width :: Int
    , repeats :: Int
    , leftDemand :: Bool
    , rightDemand :: Bool
    , variant :: Int
    } deriving (Eq, Read, Show)

data Result = Result String Outcome deriving (Eq, Read, Show)
data Outcome
    = Returned String (Maybe Integer)
    | Raised
    deriving (Eq, Read, Show)

-- A finite, representation-independent demand can cross the process seam even
-- when the two implementations define different Haskell representation types.
data Plan = SkipP | NeedP Plan | ConP String [Plan] | IntP Int
    deriving (Eq, Read, Show)

data DemandBatch = DemandBatch
    { batchLabel :: String
    , batchBudget :: Int
    , batchSeed :: Int
    , originalPlan :: Plan
    , shrinkPlans :: [Plan]
    } deriving (Eq, Read, Show)

data DemandObservation = DemandObservation
    { observationLabel :: String
    , observedPlan :: Plan
    , observedTicks :: Integer
    } deriving (Eq, Read, Show)

data Request = Draw Int Int Sample | Evaluate Sample [DemandBatch]
    deriving (Read, Show)
data Response = Drawn [DemandBatch] | Evaluated [Result] [DemandObservation]
    deriving (Eq, Read, Show)
