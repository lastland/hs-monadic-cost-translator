{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module TypeOnlyControl where

import qualified Memorist.Memorist as M

representationConstraint :: M.Repr a => a -> a
representationConstraint input = input

demandConstraint :: M.Demand a => M.Th a -> M.Th a
demandConstraint input = input

scalarConstraint :: Num a => a -> a
scalarConstraint input = input

annotation :: M.Annotated a -> M.Annotated a
annotation input = input

accounting :: M.Annot -> M.Annot
accounting input = input
