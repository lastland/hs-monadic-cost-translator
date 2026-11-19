{-# LANGUAGE TypeFamilies #-}
module RecurOtherUse where
import qualified Memorist.Memorist as M

data Chain a = End a | More (Chain a)
instance Eq a => M.Repr (Chain a) where
    type Rep (Chain a) = Chain a
    reprM (More xs) = More <$> M.reprM xs
    reprM end@(End x) = let unused = x == x in pure end
    abstract = id

bad :: Chain [Int] -> M.MemoristM (Chain [Int])
bad = M.reprM
