module Case (probes) where

import Control.Monad (replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import ReferenceSupport

probes :: [Probe]
probes = concatMap check
    [ ("alias", Source.caseAlias, caseAliasM), ("parenthesized", Source.caseParenthesized, caseParenthesizedM)
    , ("annotated", Source.caseAnnotated, caseAnnotatedM), ("as-pattern", Source.caseAs, caseAsM)
    ]
  where
    check (label, source, make) =
        [ demanded (label ++ "/selected") project demand $ \sample -> do
            x <- M.lazy (replicateM_ (width sample + 1) M.tick >> pure (number sample))
            make x
        , scalarSource (label ++ "/source")
            (\sample -> let (a,b) = source (number sample) in a+b) $ \sample -> do
            M.PairT a b <- M.fromPure1 make (number sample)
            (+) <$> M.force a <*> M.force b
        , scalarSource (label ++ "/source-skipped-error")
            (\_ -> case source (error "source subject should be unused") of (_,_) -> 1) $ \_ -> do
            x <- M.lazy (error "unused case subject evaluated" :: M.MemoristM Int)
            M.PairT _ _ <- make x
            pure 1
        , demanded (label ++ "/skipped-error") (const (const ()))
            (const (const (M.PairD M.Skip M.Skip))) $ \_ -> do
            x <- M.lazy (error "unused case subject evaluated" :: M.MemoristM Int)
            make x
        , scalar (label ++ "/repeated") $ \sample -> do
            x <- M.lazy (replicateM_ (width sample + 1) M.tick >> pure (other sample))
            M.PairT a b <- make x
            let one index = do
                    first <- M.force (if even (index + variant sample) then a else b)
                    second <- M.force (if even (index + variant sample) then b else a)
                    pure (first + second)
            sum <$> mapM one [1 .. repeats sample]
        , scalarSource (label ++ "/fixed-repeated")
            (\_ -> let (a,b) = source 5 in a+b+b) $ \_ -> do
            x <- M.lazy (M.tick >> pure (5 :: Int))
            M.PairT a b <- make x
            av <- M.force a
            bv <- M.force b
            again <- M.force b
            pure (av + bv + again)
        , failure (label ++ "/demanded-error") $ \_ -> do
            x <- M.lazy (error "demanded case subject")
            M.PairT _ b <- make x
            M.force b
        , snapshot (label ++ "/preparation-resources") (const (aliases make))
        ]

project :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
project sample (M.PairT a b) =
    (if leftDemand sample then Just (M.tval a) else Nothing,
     if rightDemand sample then Just (M.tval b) else Nothing)

demand :: Sample -> M.PairT Int Int -> M.PairD Int Int
demand sample (M.PairT a b) = M.PairD
    (if leftDemand sample then M.fullDemand a else M.Skip)
    (if rightDemand sample then M.fullDemand b else M.Skip)

aliases :: (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> (Bool, Bool, Bool, Integer)
aliases make =
    let program = do
            x <- M.lazy (error "alias observation forced its subject")
            pair <- make x
            pure (M.tname x, pair)
        (result, next) = M.runM program M.nzero
        (input, M.PairT a b) = M.value result
    in (M.tname a == input, M.tname b == input, next == M.nnext input,
        MT.toInteger (M.costAnnot result))
