{-# LANGUAGE TypeFamilies #-}
module FieldHidden where
import FieldUnused (Box(..))
import qualified Memorist.Memorist as M

data Hidden = Hidden
instance M.Repr Hidden where
    type Rep Hidden = Hidden
    reprM h = let unused = (M.reprM (Box [True]) :: M.MemoristM (Box [Bool])) in pure h
    abstract = id
