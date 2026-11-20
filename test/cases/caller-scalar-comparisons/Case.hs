module Case (probes) where

import Control.Monad (replicateM_)
import qualified Example as Source
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport

priced :: Int -> a -> M.MemoristM a
priced ticks value = replicateM_ ticks M.tick >> pure value

call :: (M.Th a -> M.Th a -> M.MemoristM b) ->
    M.MemoristM a -> M.MemoristM a -> M.MemoristM b
call function left right = do
    x <- M.lazy left
    y <- M.lazy right
    function x y

booleanSource :: String -> (Sample -> Bool) -> (Sample -> M.MemoristM Bool) -> Probe
booleanSource label source program =
    scalarSource label (fromEnum . source) (fmap fromEnum . program)

comparisons :: [(String, Int -> Int -> Bool, M.Th Int -> M.Th Int -> M.MemoristM Bool)]
comparisons =
    [("eq", Source.equal, equalM), ("ne", Source.different, differentM),
     ("lt", Source.less, lessM), ("le", Source.lessEqual, lessEqualM),
     ("gt", Source.greater, greaterM), ("ge", Source.greaterEqual, greaterEqualM),
     ("ord-superclass", Source.equalViaOrd, equalViaOrdM)]

probes :: [Probe]
probes =
    [booleanSource label (\s -> source (number s) (other s)) $ \s ->
        call translated (priced (repeats s) (number s)) (priced 3 (other s))
    | (label, source, translated) <- comparisons] ++
    [failure (label ++ "/bottom-" ++ side) $ \_ ->
        fromEnum <$> call translated
            (priced 2 (if side == "left" then undefined else 1))
            (priced 3 (if side == "right" then undefined else 1))
    | (label, _, translated) <- comparisons, side <- ["left", "right"]] ++
    [ booleanSource "integer" (\s -> Source.equal (toInteger (number s)) (toInteger (other s))) $ \s ->
        call equalM (priced 2 (toInteger (number s))) (priced 3 (toInteger (other s)))
    , booleanSource "bool" (\s -> Source.less (leftDemand s) (rightDemand s)) $ \s ->
        call lessM (priced 2 (leftDemand s)) (priced 3 (rightDemand s))
    , booleanSource "char" (\s -> Source.greaterEqual (if leftDemand s then 'a' else 'z') 'm') $ \s ->
        call greaterEqualM (priced 2 (if leftDemand s then 'a' else 'z')) (priced 3 'm')
    , booleanSource "float" (\s -> Source.lessEqual (fromIntegral (number s) :: Float) (fromIntegral (other s))) $ \s ->
        call lessEqualM (priced 2 (fromIntegral (number s) :: Float)) (priced 3 (fromIntegral (other s)))
    , booleanSource "double" (\s -> Source.different (fromIntegral (number s) :: Double) (fromIntegral (other s))) $ \s ->
        call differentM (priced 2 (fromIntegral (number s) :: Double)) (priced 3 (fromIntegral (other s)))
    , boolean "unit" $ \_ -> call equalM (priced 2 ()) (priced 3 ())
    , boolean "short-circuit-skips-operands" $ \_ -> do
        flag <- M.lazy (priced 2 False)
        call (skippedM flag) (error "left action entered" :: M.MemoristM Int) (error "right action entered")
    , demanded "suspended-outer" (const isJustT) (const M.minDemand) $ \_ ->
        call suspendedM (error "left action entered" :: M.MemoristM Int) (error "right action entered")
    , demanded "suspended-full" (const (M.abstract :: M.MaybeT Bool -> Maybe Bool)) (const M.fullDemand) $ \s ->
        call suspendedM (priced 2 (number s)) (priced 3 (other s))
    , scalarSource "work/three-two-entries" (const (Source.work 3)) $ \_ -> M.fromPure1 workM (3 :: Int)
    , scalarSource "work/varied" (Source.work . number) $ \s -> M.fromPure1 workM (number s)
    ]
  where
    isJustT M.NothingT = False
    isJustT (M.JustT _) = True
