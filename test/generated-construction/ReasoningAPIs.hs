module ReasoningAPIs where

import qualified Memorist.Memorist as M

costed :: M.MemoristM Int -> M.Cost
costed program = M.infcost program M.fullDemand

costedDemand :: M.MemoristM Int -> M.Cost
costedDemand program = M.infcost' program (M.fullDemand (0 :: Int))

annotatedCost :: M.Annotated Int -> M.Cost
annotatedCost result = M.calcCost result (M.fullDemand (0 :: Int))

checked :: M.MemoristM Int -> Bool
checked program = M.correct program (3 :: Int)

annotation :: M.Annotated Int -> M.Annot
annotation = M.annot

cost :: M.Annotated Int -> M.Cost
cost = M.costAnnot

usage :: M.Annotated Int -> M.USet
usage = M.usageAnnot
