module Case (probes) where

import Generated
import Control.Monad (replicateM_)
import qualified Example as Source
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "named/value" (Source.named . number) (M.fromPure1 namedM . number)
    , scalarSource "named/zero" (const (Source.named 0)) (const (M.fromPure1 namedM (0 :: Int)))
    , scalarSource "named/min-bound" (const (Source.named minBound)) (const (M.fromPure1 namedM (minBound :: Int)))
    , scalarSource "named/max-bound" (const (Source.named maxBound)) (const (M.fromPure1 namedM (maxBound :: Int)))
    , integer "integer/value" (\s -> M.fromPure1 largeM (fromIntegral (number s) :: Integer))
    , integer "integer/large-positive" (const (M.fromPure1 largeM (2 ^ (100 :: Int) :: Integer)))
    , integer "integer/large-negative" (const (M.fromPure1 largeM (negate (2 ^ (100 :: Int)) :: Integer)))
    , scalarSource "resolved-user-negate/value" (Source.shadowed . number) (M.fromPure1 shadowedM . number)
    , scalarSource "repeated/value" (Source.repeated . number) (M.fromPure1 repeatedM . number)
    , scalar "repeated/shared-costed-input" $ \s -> do
        x <- M.lazy (replicateM_ (width s + 1) M.tick >> pure (number s))
        repeatedM x
    , demanded "mapped/finite" (const (M.abstract :: L.ListT Int -> [Int])) (const M.fullDemand) (M.fromPure1 mappedM . numbers)
    , scalarSource "mapped/partial-tail" (mappedHead . number) $ \s -> do
        values <- M.fromPure1 mappedM (number s : undefined)
        case values of
            L.ConsT x _ -> M.force x
            L.NilT -> error "expected a nonempty mapped list"
    , scalar "mapped/skips-bottom-element" $ \_ -> do
        values <- M.fromPure1 mappedM ((undefined :: Int) : undefined)
        case values of
            L.ConsT _ _ -> pure 1
            L.NilT -> error "expected a nonempty mapped list"
    , scalarSource "composed/value" (Source.composed . number) (M.fromPure1 composedM . number)
    , scalarSource "ignored/failing-operand" (Source.ignored . number) (M.fromPure1 ignoredM . number)
    , failure "named/demanded-bottom" (const (M.fromPure1 namedM (undefined :: Int)))
    , failure "composed/demanded-bottom" (const (M.fromPure1 composedM (undefined :: Int)))
    ]

mappedHead :: Int -> Int
mappedHead value = case Source.mapped (value : undefined) of
    first : _ -> first
    [] -> error "expected a nonempty mapped list"
