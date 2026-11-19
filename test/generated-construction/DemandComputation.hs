{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module DemandComputation where

import qualified DemandControl as D
import qualified Memorist.Memorist as M

leak :: M.Th Int -> M.ThD Int -> M.MemoristM [M.ThD Int]
leak input demand = pure (D.shrinkView input demand)
