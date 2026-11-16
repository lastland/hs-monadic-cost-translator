module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "polymorphic-local" (Source.local . number) (\s -> M.fromPure1 localM (number s))
    , scalar "unused-bottom-input" (\_ -> M.fromPure1 localM (undefined :: Int))
    , demanded "empty-integer-list" (\_ _ -> ([] :: [Int])) (\_ _ -> L.NilD)
        (\s -> M.fromPure1 emptyM (number s) :: M.MemoristM (L.ListT Int))
    , demanded "empty-boolean-list" (\_ _ -> ([] :: [Bool])) (\_ _ -> L.NilD)
        (\_ -> M.fromPure1 emptyM (undefined :: Int) :: M.MemoristM (L.ListT Bool))
    , scalar "ints-empty" (\_ -> M.fromPure1 intsM ([] :: [Int]))
    , scalar "bools-empty" (\_ -> M.fromPure1 boolsM ([] :: [Bool]))
    , failure "ints-nonempty" (\s -> M.fromPure1 intsM [number s])
    , failure "bools-nonempty" (\s -> M.fromPure1 boolsM [leftDemand s])
    ]
