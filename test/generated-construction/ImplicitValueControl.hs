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
