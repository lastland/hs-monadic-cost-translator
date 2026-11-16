-- | The wire format is shared by both isolated observers and the comparator.
module Protocol (Sample(..), Result(..), Outcome(..)) where

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
