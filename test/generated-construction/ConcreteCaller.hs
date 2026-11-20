module ConcreteCaller where
import qualified Memorist.Memorist as M
import qualified QuantifiedApplication as Q

makeInt :: Q.Higher Maybe Int -> M.MemoristM (Q.HigherT M.MaybeT Int)
makeInt = M.reprM
makeBool :: Q.Higher Maybe Bool -> M.MemoristM (Q.HigherT M.MaybeT Bool)
makeBool = M.reprM
