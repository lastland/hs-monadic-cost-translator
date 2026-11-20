module StockResetUnsafe where

import qualified Memorist.Memorist as M

data Observing = Observing (M.Th Int)
instance Eq Observing where
    Observing x == Observing y = M.tval x == M.tval y
instance Show Observing where
    showsPrec n (Observing x) = showsPrec n (M.tval x)

data ResetUnsafeD a = ResetUnsafeD (M.ThD (ResetUnsafeD Int)) (M.ThD a) (M.ThD Observing)
    deriving (Eq, Show)

sameReset :: ResetUnsafeD Bool -> ResetUnsafeD Bool -> Bool
sameReset = (==)
resetTop :: ResetUnsafeD Bool -> String
resetTop = show
