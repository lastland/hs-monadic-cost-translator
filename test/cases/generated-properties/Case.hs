module Case (probes) where

import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..), ListD(..))
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "bump" (S.bump . number) (\s -> M.fromPure1 bumpM (number s))
    , scalarSource "add" (\s -> S.add (number s) (other s)) (\s -> M.fromPure2 addM (number s) (other s))
    , scalarSource "folded" (S.folded . numbers) (\s -> M.fromPure1 foldedM (numbers s))
    , demanded "mapped-full" (\_ -> M.abstract :: ListT Int -> [Int]) (\_ -> M.fullDemand) mapped
    , demanded "mapped-partial" view demand mapped
    , demanded "stream-prefix" view demand (\s -> M.fromPure1 streamM (number s))
    , demanded "stream-first-three" (\_ -> listView 3 True True) (\_ -> listDemand 3 True True)
        (\s -> M.fromPure1 streamM (number s))
    , demanded "mapped-stream-prefix" view demand $ \s -> do
        stream <- M.lazy (M.fromPure1 streamM (other s))
        mappedM stream
    , scalarSource "mapped-head-sharing" (\s -> case S.mapped (numbers s) of [] -> 0; x:_ -> x + x) $ \s -> do
        list <- mapped s
        case list of
            NilT -> pure 0
            ConsT headThunk _ -> do
                a <- M.force headThunk
                b <- M.force headThunk
                pure (a + b)
    ]
  where
    mapped s = M.fromPure1 mappedM (numbers s)
    view s = listView (width s) (leftDemand s) (rightDemand s)
    demand s = listDemand (width s) (leftDemand s) (rightDemand s)

-- Observe only the selected heads and at most the selected finite spine. At
-- width zero the result's outer constructor is still the program's WHNF.
listView :: Int -> Bool -> Bool -> ListT Int -> [Maybe Int]
listView k left right xs
    | k <= 0 = []
    | otherwise = case xs of
        NilT -> []
        ConsT x rest ->
            (if left then Just (M.tval x) else Nothing) :
                (if k == 1 then [] else listView (k - 1) right left (M.tval rest))

listDemand :: Int -> Bool -> Bool -> ListT Int -> ListD Int
listDemand k left right xs = case xs of
    NilT -> NilD
    ConsT x rest -> ConsD
        (if k > 0 && left then M.Need (M.tval x) else M.Skip)
        (if k > 1 then M.Need (listDemand (k - 1) right left (M.tval rest)) else M.Skip)
