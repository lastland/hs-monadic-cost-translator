module Case (probes) where
import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "ordinary-Higher-observed" (S.observed . number)
        (\s -> M.fromPure1 observedM (number s))
    , scalarSource "ordinary-Higher-first" (\s -> S.first (S.wrap undefined [other s]))
        (\s -> do
            scalar <- M.lazy (error "unused scalar" :: M.MemoristM Int)
            applied <- M.lazy (M.reprM [other s])
            input <- M.lazy (wrapM scalar applied)
            firstM input)
    , failure "demanded-observed-bottom"
        (\_ -> M.lazy (error "demanded input" :: M.MemoristM Int) >>= observedM)
    , demanded "Maybe-Int-representation" (\_ -> viewInt) (\_ -> M.fullDemand)
        (\s -> M.reprM (S.Higher (number s) (Just (other s))) :: M.MemoristM (HigherT M.MaybeT Int))
    , demanded "Maybe-Bool-representation" (\_ -> viewBool) (\_ -> M.fullDemand)
        (\s -> M.reprM (S.Higher (leftDemand s) (Just (rightDemand s))) :: M.MemoristM (HigherT M.MaybeT Bool))
    , demanded "Maybe-skipped-bottom-fields" (\_ _ -> "Higher") (\_ _ -> HigherD M.Skip M.Skip)
        (\_ -> M.reprM (S.Higher (error "skipped scalar" :: Int) (error "skipped applied" :: Maybe Int))
            :: M.MemoristM (HigherT M.MaybeT Int))
    , demanded "wrap-Maybe-Int" (\_ -> viewInt) (\_ -> M.fullDemand)
        (\s -> M.fromPure2 wrapM (number s) (Just (other s)))
    , snapshot "Eq-Show-Maybe-Int" $ \s ->
        let d = HigherD (M.Need (number s)) (M.Need (M.JustD (M.Need (other s))))
                :: HigherD M.MaybeD Int
            empty = HigherD M.Skip (M.Need M.NothingD) :: HigherD M.MaybeD Int
        in (d == d, d /= empty, show d, showsPrec 11 d "", showList [d, empty] "")
    , snapshot "Eq-Show-Maybe-Bool" $ \s ->
        let d = HigherD (M.Need (leftDemand s)) (M.Need (M.JustD (M.Need (rightDemand s))))
                :: HigherD M.MaybeD Bool
            empty = HigherD M.Skip (M.Need M.NothingD) :: HigherD M.MaybeD Bool
        in (d == d, d /= empty, show d, showsPrec 11 d "", showList [d, empty] "")
    ]

viewInt :: HigherT M.MaybeT Int -> (Int, Maybe Int)
viewInt (HigherT a b) = (M.tval a, M.abstract (M.tval b))
viewBool :: HigherT M.MaybeT Bool -> (Bool, Maybe Bool)
viewBool (HigherT a b) = (M.tval a, M.abstract (M.tval b))
