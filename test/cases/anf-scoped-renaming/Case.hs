module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "caseParameter/Int" (\_ -> Source.caseParameter (undefined :: Int)) $ \_ ->
        M.fromPure1 caseParameterM (undefined :: Int)
    , scalarSource "caseParameter/Bool" (\_ -> Source.caseParameter (undefined :: Bool)) $ \_ ->
        M.fromPure1 caseParameterM (undefined :: Bool)
    , scalarSource "guardParameter/Int" (\_ -> Source.guardParameter (undefined :: Int)) $ \_ ->
        M.fromPure1 guardParameterM (undefined :: Int)
    , scalarSource "guardParameter/Bool" (\_ -> Source.guardParameter (undefined :: Bool)) $ \_ ->
        M.fromPure1 guardParameterM (undefined :: Bool)
    , scalarSource "caseFactory/Int" (\s -> Source.caseFactory (number s)) $ \s ->
        M.fromPure1 caseFactoryM (number s)
    , scalarSource "caseFactory/Integer" (\s -> Source.caseFactory (toInteger (number s))) $ \s ->
        M.fromPure1 caseFactoryM (toInteger (number s))
    , scalarSource "guardFactory/Int" (\s -> Source.guardFactory (other s)) $ \s ->
        M.fromPure1 guardFactoryM (other s)
    , scalarSource "guardFactory/Integer" (\s -> Source.guardFactory (toInteger (other s))) $ \s ->
        M.fromPure1 guardFactoryM (toInteger (other s))
    , scalarSource "parameter/case-finite-int" (\s -> Source.caseParameter (number s)) $ \s ->
        M.fromPure1 caseParameterM (number s)
    , scalarSource "parameter/case-finite-bool" (\s -> Source.caseParameter (leftDemand s)) $ \s ->
        M.fromPure1 caseParameterM (leftDemand s)
    , scalarSource "parameter/guard-finite-int" (\s -> Source.guardParameter (other s)) $ \s ->
        M.fromPure1 guardParameterM (other s)
    , scalarSource "parameter/guard-finite-bool" (\s -> Source.guardParameter (rightDemand s)) $ \s ->
        M.fromPure1 guardParameterM (rightDemand s)
    , scalarSource "factory/case-bottom-int" (\_ -> Source.caseFactory (undefined :: Int)) $ \_ ->
        M.fromPure1 caseFactoryM (undefined :: Int)
    , scalarSource "factory/case-bottom-integer" (\_ -> Source.caseFactory (undefined :: Integer)) $ \_ ->
        M.fromPure1 caseFactoryM (undefined :: Integer)
    , scalarSource "factory/guard-bottom-int" (\_ -> Source.guardFactory (undefined :: Int)) $ \_ ->
        M.fromPure1 guardFactoryM (undefined :: Int)
    , scalarSource "factory/guard-bottom-integer" (\_ -> Source.guardFactory (undefined :: Integer)) $ \_ ->
        M.fromPure1 guardFactoryM (undefined :: Integer)
    , scalarSource "factory/int-repeated"
        (\s -> sum (replicate (repeats s) (Source.caseFactory (number s) + Source.guardFactory (number s)))) $ \s -> do
        argument <- M.lazy (pure (number s))
        observations <- replicateM (repeats s) $ do
            a <- caseFactoryM argument
            b <- guardFactoryM argument
            pure (a + b)
        pure (sum observations)
    , scalarSource "factory/integer-repeated"
        (\s -> sum (replicate (repeats s) (Source.caseFactory (toInteger (other s)) + Source.guardFactory (toInteger (other s))))) $ \s -> do
        argument <- M.lazy (pure (toInteger (other s)))
        observations <- replicateM (repeats s) $ do
            a <- caseFactoryM argument
            b <- guardFactoryM argument
            pure (a + b)
        pure (sum observations)
    , scalarSource "factory/mixed-dictionaries" mixedSource $ \s -> do
        int <- M.lazy (error "factory forced ignored Int argument" :: M.MemoristM Int)
        integer <- M.lazy (error "factory forced ignored Integer argument" :: M.MemoristM Integer)
        let intCalls = do
                a <- caseFactoryM int
                b <- guardFactoryM int
                pure (a + b)
            integerCalls = do
                a <- guardFactoryM integer
                b <- caseFactoryM integer
                pure (a + b)
        observations <- replicateM (repeats s) $
            if even (variant s)
                then do a <- intCalls; b <- integerCalls; pure (a + b)
                else do b <- integerCalls; a <- intCalls; pure (a + b)
        pure (sum observations)
    , scalarSource "work/int" (\s -> Source.work (number s)) $ \s ->
        M.fromPure1 workM (number s)
    , scalarSource "work/integer" (\s -> fromInteger (Source.work (toInteger (other s)))) $ \s ->
        fromInteger <$> M.fromPure1 workM (toInteger (other s))
    ]

mixedSource s = sum $ replicate (repeats s) $ sum
    [ Source.caseFactory (undefined :: Int)
    , Source.guardFactory (undefined :: Int)
    , Source.guardFactory (undefined :: Integer)
    , Source.caseFactory (undefined :: Integer)
    ]
