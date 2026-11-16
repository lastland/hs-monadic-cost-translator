module Case (probes) where

import Generated
import ReferenceSupport
import Memorist.Data.List (ListT(..), ListD(..))
import qualified Example as Source
import qualified Memorist.Memorist as M

probes :: [Probe]
probes =
    [ scalarSource "entry" (\sample -> Source.observed (Just (number sample)))
          (\sample -> M.fromPure1 observedM (Just (number sample)))
    , scalarSource "other input" (\sample -> Source.observed (Just (other sample)))
          (\sample -> M.fromPure1 observedM (Just (other sample)))
    , demanded "flat full" (\_ -> (M.abstract :: ListT Int -> [Int])) (\_ -> M.fullDemand)
          (\sample -> M.fromPure1 flatM (selected sample))
    , demanded "flat selected fields" project demand
          (\sample -> M.fromPure1 flatM (selected sample))
    , scalarSource "empty head fails" (\_ -> Source.observed (Nothing :: Maybe Int))
          (\_ -> M.fromPure1 observedM (Nothing :: Maybe Int))
    ]
  where
    selected sample = if even (variant sample) then Just (number sample) else Nothing
    project _ NilT = (Nothing, Nothing)
    project sample (ConsT value rest) =
        ( Just (if leftDemand sample then Just (M.tval value) else Nothing)
        , if rightDemand sample then Just (case M.tval rest of
              NilT -> False
              ConsT{} -> True) else Nothing
        )
    demand _ NilT = NilD
    demand sample (ConsT value _) = ConsD
        (if leftDemand sample then M.Need (M.tval value) else M.Skip)
        (if rightDemand sample then M.Need NilD else M.Skip)
