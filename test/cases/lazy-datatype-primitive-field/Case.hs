module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "boxed" (Source.boxed . number) (\s -> M.fromPure1 boxedM (number s))
    , demanded "wrapped-constructor" (\_ _ -> "Box") (\_ _ -> BoxD M.Skip M.Skip)
        (\_ -> M.fromPure1 wrapM (undefined :: Int))
    , demanded "wrapped-first-field"
        (\_ (BoxT x _) -> M.tval x)
        (\_ (BoxT x _) -> BoxD (M.Need (M.tval x)) M.Skip)
        (\s -> M.fromPure1 wrapM (number s))
    , demanded "wrapped-field-demands" (\s -> project (leftDemand s) (rightDemand s))
        (\s -> boxDemand (leftDemand s) (rightDemand s)) (\s -> M.fromPure1 wrapM (other s))
    , failure "primitive-field-is-bottom" $ \s -> do
        BoxT _ n <- M.fromPure1 wrapM (number s)
        M.force n
    , failure "first-field-is-demanded" (\_ -> M.fromPure1 boxedM (undefined :: Int))
    ]

project :: Bool -> Bool -> BoxT Int -> (Maybe Int, Maybe Int)
project wantX wantN (BoxT x n) =
    (if wantX then Just (M.tval x) else Nothing,
     if wantN then Just (M.tval n) else Nothing)

boxDemand :: Bool -> Bool -> BoxT Int -> BoxD Int
boxDemand wantX wantN (BoxT x n) = BoxD
    (if wantX then M.Need (M.tval x) else M.Skip)
    (if wantN then M.Need (M.tval n) else M.Skip)
