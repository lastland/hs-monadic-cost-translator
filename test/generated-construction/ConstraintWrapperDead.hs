{-# LANGUAGE ConstraintKinds, RankNTypes, TypeFamilies #-}
module ConstraintWrapperDead where

import ConstraintWrapperControl (permit)
import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M

data Source = Source Int
data Target = Target (M.Th Int)

instance M.Repr Source where
    type Rep Source = Target
    reprM (Source input) = Target <$> M.lazy (M.reprM input)
    abstract (Target field) = permit (Proxy :: Proxy (M.Repr Int))
        (let unused = M.value (M.eval1 (pure 17 :: M.MemoristM Int))
         in Source (M.abstract (M.tval field)))
