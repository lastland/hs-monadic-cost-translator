{-# LANGUAGE TypeFamilies #-}
module BoxSupportSeparation where

import qualified Memorist.Memorist as M

data Source = Source (M.Th Int)
data Observation = Observation (M.Th Int)

instance M.Repr Source where
    type Rep Source = Int
    reprM (Source x) = pure (M.tval x)
    abstract _ = Source undefined

instance M.Demand Observation where
    type Dem Observation = Int
    fullDemand (Observation x) = M.tval x
    usage _ _ = M.empty

instance Eq Observation where
    Observation x == Observation y = M.tval x == M.tval y

nested :: Observation -> Int
nested _ = let unused = M.tval (undefined :: M.Th Int) in 0

alias = M.abstract
erased x = const (pure () :: M.MemoristM ()) (alias x :: Source)
