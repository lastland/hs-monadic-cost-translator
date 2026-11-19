module ParamReprUse where

import ParamRepr
import qualified Memorist.Memorist as M

box :: Box Int -> M.MemoristM (Box Int)
box = M.reprM

nested :: Maybe (Outer Bool Int) -> M.MemoristM (M.MaybeT (Outer Bool Int))
nested = M.reprM
