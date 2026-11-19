module Case (probes) where

import qualified Example as Source
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "infix guard" (\s -> Source.guarded (number s) (other s))
        (\s -> M.fromPure2 guardedM (number s) (other s))
    , scalarSource "prefix if" (\s -> Source.prefixIf (number s) (other s))
        (\s -> M.fromPure2 prefixIfM (number s) (other s))
    , scalarSource "ordered comparison guards" (\s -> Source.threeWay (number s) (other s))
        (\s -> M.fromPure2 threeWayM (number s) (other s))
    , scalarSource "shadowed otherwise" (\s -> Source.shadowed (leftDemand s) (rightDemand s))
        (\s -> M.fromPure2 shadowedM (leftDemand s) (rightDemand s))
    , failure "first guard precedes shadowed otherwise"
        (const (M.fromPure2 shadowedM (undefined :: Bool) False))
    , scalarSource "known conditions" (Source.known . number)
        (M.fromPure1 knownM . number)
    , demanded "partial comparison in map" (const (M.abstract :: L.ListT Bool -> [Bool]))
        (const M.fullDemand) $ \s -> do
            lower <- M.lazy (M.tick >> pure (number s))
            xs <- M.lazy (M.reprM (numbers s))
            mappedM lower xs
    , scalar "and skips bottom" (const (withBottom andIfM False))
    , failure "and needs bottom" (const (withBottom andIfM True))
    , scalar "or skips bottom" (const (withBottom orGuardM True))
    , failure "or needs bottom" (const (withBottom orGuardM False))
    ]

-- The tick makes an unnecessary force observable even if the operator never
-- inspects the right-hand value after forcing its thunk.
withBottom :: (M.Th Bool -> M.Th Bool -> M.MemoristM Int) -> Bool -> M.MemoristM Int
withBottom choose left = do
    x <- M.lazy (pure left)
    y <- M.lazy (M.tick >> pure (error "demanded right operand"))
    choose x y
