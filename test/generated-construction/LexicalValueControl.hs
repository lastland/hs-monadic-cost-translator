{-# LANGUAGE TypeFamilies #-}
module LexicalValueControl where

import LexicalFamilyEvidence (Result)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

type instance Result () = Int

law :: M.Th Int -> Q.Property
law t = let observed :: Result ()
            observed = M.tval t
        in Q.property (observed == (3 :: Int))

reachable :: Q.Property
reachable = law (M.value (M.eval1 (M.lazy (pure (3 :: Int)) :: M.MemoristM (M.Th Int))))

generic :: Eq a => M.Th a -> a -> Q.Property
generic t want = let observed = M.tval t in Q.property (observed == want)

sample :: M.Th [a] -> Q.Gen [a]
sample t = let readings = M.tval t in Q.elements [readings]
