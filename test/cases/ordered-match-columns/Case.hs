module Case (probes) where

import Control.Monad (replicateM_)
import qualified Example as Source
import Generated (barrierM, chooseM, scopedM, keepM)
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ middle "middle/first-row-skips-bottom-limit" $ \s ->
          (undefined, Just (0, number s), True)
    , middle "middle/failed-guard-uses-later-row" $ \s ->
          (number s, Just (0, number s + 1), False)
    , middle "middle/failed-nested-pattern-skips-bottom-guard" $ \s ->
          (other s, Just (1 + abs (number s), other s), undefined)
    , middle "middle/just-falls-through" $ \s ->
          (number s, Just (1, number s - 1), undefined)
    , middle "middle/nothing-wins" (const (undefined, Nothing, True))
    , middle "middle/nothing-falls-through" $ \s -> (number s, Nothing, False)
    , snapshot "middle/row-aliases" $ \s ->
          (rowAliases True undefined, rowAliases False (abs (number s)))
    , barriers "barrier/guard-skips-both-bottoms" (const (True, undefined, undefined))
    , barriers "barrier/right-skips-bottom-left" $ \s ->
          (False, undefined, Just (number s))
    , barriers "barrier/left-before-final-nothing" $ \s ->
          (False, Just (number s), Nothing)
    , barriers "barrier/both-nothing" (const (False, Nothing, Nothing))
    , compare3 Source.scoped scopedM "fallthrough/guard-local-does-not-capture-outer" $ \s ->
          if leftDemand s then (undefined, number s, True) else (number s, undefined, False)
    , scalarSource "fallthrough/shared-field-does-not-capture-argument"
        (\s -> Source.keep (number s) (Just 7))
        (\s -> M.fromPure2 keepM (number s) (Just (7 :: Int)))
    , failure "barrier/must-inspect-right-before-a-matching-left" $ \s ->
          M.fromPure3 barrierM False (Just (number s)) (undefined :: Maybe Int)
    , failure "middle/must-inspect-nested-pair" $ \s ->
          M.fromPure3 selectedM (number s) (Just (undefined :: (Int, Int))) False
    ]
  where
    middle = compare3 (\a b c -> snd (Source.choose a b c)) selectedM
    barriers = compare3 Source.barrier barrierM

selectedM :: M.Th Int -> M.Th (M.MaybeT (M.PairT Int Int)) -> M.Th Bool -> M.MemoristM Int
selectedM limit middle enabled = do
    M.PairT _ selected <- chooseM limit middle enabled
    M.force selected

-- Distinct input costs expose a speculative force even if its value is ignored.
compare3 :: (M.Repr a, M.Repr b, M.Repr c) =>
    (a -> b -> c -> Int) ->
    (M.Th (M.Rep a) -> M.Th (M.Rep b) -> M.Th (M.Rep c) -> M.MemoristM Int) ->
    String -> (Sample -> (a, b, c)) -> Probe
compare3 source call label inputs = scalarSource label expected $ \s -> do
    let (x, y, z) = inputs s
    a <- M.lazy (M.tick >> M.reprM x)
    b <- M.lazy (replicateM_ 2 M.tick >> M.reprM y)
    c <- M.lazy (replicateM_ 4 M.tick >> M.reprM z)
    call a b c
  where
    expected s = let (x, y, z) = inputs s in source x y z

-- Compare alias relationships, independent of the allocator's choice of names.
rowAliases :: Bool -> Int -> (Bool, Bool)
rowAliases takeFirst n = M.value (M.eval1 $ do
    limit <- M.lazy (pure 0)
    key <- M.lazy (pure 0)
    payload <- M.lazy (pure n)
    middle <- M.lazy (M.JustT <$> M.lazy (pure (M.PairT key payload)))
    flag <- M.lazy (pure takeFirst)
    M.PairT original selected <- chooseM limit middle flag
    pure (M.tname original == M.tname middle,
          M.tname selected == M.tname (if takeFirst then payload else key)))
