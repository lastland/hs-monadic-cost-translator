module Case (probes) where

import Control.Monad (replicateM)
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified ScopedAnnotations as Source

type IntFunction = Bindings -> M.Th Int -> M.MemoristM Int
type IntegerFunction = Bindings -> M.Th Integer -> M.MemoristM Int
type IntAction = M.Th Int -> M.MemoristM Int
type IntegerAction = M.Th Integer -> M.MemoristM Int

probes :: [Probe]
-- Generated explicit foralls sit after Bindings. Instantiate after applying
-- that argument, rather than trying to store a higher-rank field in a tuple.
probes = concatMap parameterChecks
    [("case", Source.caseParameter, \bindings -> caseParameterM bindings :: IntAction),
     ("guard", Source.guardParameter, \bindings -> guardParameterM bindings :: IntAction),
     ("alpha-renamed", Source.caseParameterRenamed, \bindings -> caseParameterRenamedM bindings :: IntAction)] ++
    concatMap factoryChecks
    [("case", Source.caseFactory, \bindings -> caseFactoryM bindings :: IntAction,
      Source.caseFactory, \bindings -> caseFactoryM bindings :: IntegerAction),
     ("guard", Source.guardFactory, \bindings -> guardFactoryM bindings :: IntAction,
      Source.guardFactory, \bindings -> guardFactoryM bindings :: IntegerAction),
     ("alpha-renamed", Source.guardFactoryRenamed, \bindings -> guardFactoryRenamedM bindings :: IntAction,
      Source.guardFactoryRenamed, \bindings -> guardFactoryRenamedM bindings :: IntegerAction)] ++
    [ scalarSource "parameter/case-bottom-bool" (const (Source.caseParameter (undefined :: Bool))) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (caseParameterM bindings) (undefined :: Bool)
    , scalarSource "parameter/guard-bool" (Source.guardParameter . leftDemand) $ \s -> do
        bindings <- initialize
        M.fromPure1 (guardParameterM bindings) (leftDemand s)
    , scalarSource "factory/mixed-dictionaries-and-controls" mixedSource mixedProgram
    , scalarSource "work/int" (Source.work . number) $ \s -> do
        bindings <- initialize
        M.fromPure1 (workM bindings) (number s)
    , integer "work/integer" $ \s -> do
        bindings <- initialize
        M.fromPure1 (workM bindings) (toInteger (other s))
    , scalarSource "work/zero" (const (Source.work (0 :: Int))) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (workM bindings) (0 :: Int)
    , scalarSource "ignore/bottom" (const (Source.ignore (undefined :: Int))) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (ignoreM bindings) (undefined :: Int)
    , failure "work/demanded-bottom" $ \_ -> do
        bindings <- initialize
        M.fromPure1 (workM bindings) (undefined :: Int)
    ]

parameterChecks :: (String, Int -> Int, IntFunction) -> [Probe]
parameterChecks (label, source, call) =
    [ scalarSource ("parameter/" ++ label ++ "/finite") (source . number) $ \s -> do
        bindings <- initialize
        M.fromPure1 (call bindings) (number s)
    , scalarSource ("parameter/" ++ label ++ "/bottom") (const (source undefined)) $ \_ -> do
        bindings <- initialize
        x <- M.lazy (error "captured parameter was forced")
        call bindings x
    ]

factoryChecks :: (String, Int -> Int, IntFunction, Integer -> Int, IntegerFunction) -> [Probe]
factoryChecks (label, sourceInt, callInt, sourceInteger, callInteger) =
    [ scalarSource ("factory/" ++ label ++ "/int") (sourceInt . number) $ \s -> do
        bindings <- initialize
        M.fromPure1 (callInt bindings) (number s)
    , scalarSource ("factory/" ++ label ++ "/integer") (sourceInteger . toInteger . other) $ \s -> do
        bindings <- initialize
        M.fromPure1 (callInteger bindings) (toInteger (other s))
    , scalarSource ("factory/" ++ label ++ "/bottom-int") (const (sourceInt undefined)) $ \_ -> do
        bindings <- initialize
        x <- M.lazy (error "factory forced ignored Int argument")
        callInt bindings x
    , scalarSource ("factory/" ++ label ++ "/bottom-integer") (const (sourceInteger undefined)) $ \_ -> do
        bindings <- initialize
        x <- M.lazy (error "factory forced ignored Integer argument")
        callInteger bindings x
    , scalarSource ("factory/" ++ label ++ "/repeated-int")
        (\s -> repeats s * sourceInt (number s)) $ \s -> do
        bindings <- initialize
        x <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (callInt bindings x)
    , scalarSource ("factory/" ++ label ++ "/repeated-integer")
        (\s -> repeats s * sourceInteger (toInteger (other s))) $ \s -> do
        bindings <- initialize
        x <- M.lazy (pure (toInteger (other s)))
        sum <$> replicateM (repeats s) (callInteger bindings x)
    ]

mixedSource :: Sample -> Int
mixedSource s = repeats s * sum
    [ Source.caseFactory (undefined :: Int), Source.guardFactory (undefined :: Int)
    , Source.guardFactoryRenamed (undefined :: Int)
    , Source.caseFactory (undefined :: Integer), Source.guardFactory (undefined :: Integer)
    , Source.guardFactoryRenamed (undefined :: Integer)
    ]

mixedProgram :: Sample -> M.MemoristM Int
mixedProgram s = do
    bindings <- initialize
    int <- M.lazy (error "factory forced Int input" :: M.MemoristM Int)
    integerInput <- M.lazy (error "factory forced Integer input" :: M.MemoristM Integer)
    let ints = sum <$> sequence
            [caseFactoryM bindings int, guardFactoryM bindings int, guardFactoryRenamedM bindings int]
        integers = sum <$> sequence
            [caseFactoryM bindings integerInput, guardFactoryM bindings integerInput, guardFactoryRenamedM bindings integerInput]
        one = if even (variant s)
            then (+) <$> ints <*> integers
            else (+) <$> integers <*> ints
    sum <$> replicateM (repeats s) one
