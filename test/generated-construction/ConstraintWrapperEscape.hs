{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module ConstraintWrapperEscape where

import ConstraintWrapperControl (permit)
import Data.Proxy (Proxy(..))
import qualified DemandControl as D
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

ordinary :: M.Th Int -> Int
ordinary input = permit (Proxy :: Proxy (M.Repr Int)) (M.tval input)

generator :: Int -> M.Th Int -> QC.Gen (M.ThD Int)
generator size input = permit (Proxy :: Proxy (D.Observation Int)) (D.generateField size input)
