{-# LANGUAGE ImplicitParams #-}
module ImplicitValueControl where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

direct :: M.Th Bool -> Q.Property
direct t = let ?seen = M.tval t in Q.property ?seen

selected :: (Num a, Eq a) => a -> Q.Property
selected x = let ?seen = x == 1 in Q.property ?seen

rebound :: M.Th Bool -> Q.Property
rebound t = let ?seen = False in
    let ?seen = M.tval t in Q.property ?seen

throughValue :: M.Th Bool -> Q.Property
throughValue t = let observed = M.tval t in
    let ?seen = observed in
    let flag = ?seen :: Bool in Q.property flag

reboundCall :: M.Th Bool -> Q.Property
reboundCall t = let ?seen = M.tval t in
    Q.property $ ?seen && (let ?seen = False in helper ())
  where
    helper :: (?seen :: Bool) => () -> Bool
    helper () = True

-- Both ?n declarations and both RHSs print the same spans. Only mapped
-- uses the observation; the ordinary dictionary in unusedInt is lawful.
mapped :: M.Th Bool -> Q.Property
mapped t =
{-# LINE 70 "SharedImplicit.hs" #-}
    let ?n = M.tval t in Q.property ?n

unusedInt :: Q.Property
unusedInt =
{-# LINE 70 "SharedImplicit.hs" #-}
    let ?n = (1::Int) in Q.property True
