module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "observe" (Source.observe . number) (\s -> M.fromPure1 observeM (number s))
    , scalar "second-skips-first-and-rest"
        (\s -> M.fromPure1 secondM (undefined : other s : undefined))
    , failure "second-short-list" (\s -> M.fromPure1 secondM [number s])
    , failure "second-bottom-spine" (\_ -> M.fromPure1 secondM (undefined :: [Int]))
    , demanded "finite-stream-prefix"
        (\s -> prefix (1 + width s) (leftDemand s))
        (\s -> prefixDemand (1 + width s) (leftDemand s))
        (\s -> M.fromPure1 streamM (number s))
    , demanded "bottom-head-stream-spine"
        (\s -> prefix (1 + width s) False) (\s -> prefixDemand (1 + width s) False)
        (\_ -> M.fromPure1 streamM (undefined :: Int))
    , failure "observed-bottom-head" (\_ -> M.fromPure1 observeM (undefined :: Int))
    ]

prefix :: Int -> Bool -> L.ListT Int -> [Maybe Int]
prefix n wantHead xs
    | n <= 0 = []
    | otherwise = case xs of
        L.NilT -> []
        L.ConsT h rest ->
            (if wantHead then Just (M.tval h) else Nothing) :
            prefix (n - 1) wantHead (M.tval rest)

prefixDemand :: Int -> Bool -> L.ListT Int -> L.ListD Int
prefixDemand n wantHead xs = case xs of
    L.NilT -> L.NilD
    L.ConsT h rest -> L.ConsD
        (if wantHead then M.Need (M.tval h) else M.Skip)
        (if n <= 1 then M.Skip else M.Need (prefixDemand (n - 1) wantHead (M.tval rest)))
