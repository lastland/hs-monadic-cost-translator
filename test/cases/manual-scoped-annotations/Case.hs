module Case (probes) where

import Control.Monad (replicateM)
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified ScopedAnnotations as Source

type IntFunction = M.Th Int -> M.MemoristM Int
type IntegerFunction = M.Th Integer -> M.MemoristM Int

probes :: [Probe]
-- Instantiate polymorphic functions before storing the catalog callbacks.
probes = concatMap parameterChecks
    [("case", Source.caseParameter, caseParameterM :: IntFunction),
     ("guard", Source.guardParameter, guardParameterM :: IntFunction),
     ("alpha-renamed", Source.caseParameterRenamed, caseParameterRenamedM :: IntFunction)] ++
    concatMap factoryChecks
    [("case", Source.caseFactory, caseFactoryM :: IntFunction,
      Source.caseFactory, caseFactoryM :: IntegerFunction),
     ("guard", Source.guardFactory, guardFactoryM :: IntFunction,
      Source.guardFactory, guardFactoryM :: IntegerFunction),
     ("alpha-renamed", Source.guardFactoryRenamed, guardFactoryRenamedM :: IntFunction,
      Source.guardFactoryRenamed, guardFactoryRenamedM :: IntegerFunction)] ++
    [ scalarSource "parameter/case-bottom-bool" (const (Source.caseParameter (undefined :: Bool))) $ \_ ->
        M.fromPure1 caseParameterM (undefined :: Bool)
    , scalarSource "parameter/guard-bool" (Source.guardParameter . leftDemand) $ \s ->
        M.fromPure1 guardParameterM (leftDemand s)
    , scalarSource "factory/mixed-dictionaries-and-controls" mixedSource mixedProgram
    , scalarSource "work/int" (Source.work . number) $ \s ->
        M.fromPure1 workM (number s)
    , integer "work/integer" $ \s ->
        M.fromPure1 workM (toInteger (other s))
    , scalarSource "work/zero" (const (Source.work (0 :: Int))) $ \_ ->
        M.fromPure1 workM (0 :: Int)
    , scalarSource "ignore/bottom" (const (Source.ignore (undefined :: Int))) $ \_ ->
        M.fromPure1 ignoreM (undefined :: Int)
    , failure "work/demanded-bottom" $ \_ ->
        M.fromPure1 workM (undefined :: Int)
    ]

parameterChecks :: (String, Int -> Int, IntFunction) -> [Probe]
parameterChecks (label, source, call) =
    [ scalarSource ("parameter/" ++ label ++ "/finite") (source . number) $ \s ->
        M.fromPure1 call (number s)
    , scalarSource ("parameter/" ++ label ++ "/bottom") (const (source undefined)) $ \_ -> do
        x <- M.lazy (error "captured parameter was forced")
        call x
    ]

factoryChecks :: (String, Int -> Int, IntFunction, Integer -> Int, IntegerFunction) -> [Probe]
factoryChecks (label, sourceInt, callInt, sourceInteger, callInteger) =
    [ scalarSource ("factory/" ++ label ++ "/int") (sourceInt . number) $ \s ->
        M.fromPure1 callInt (number s)
    , scalarSource ("factory/" ++ label ++ "/integer") (sourceInteger . toInteger . other) $ \s ->
        M.fromPure1 callInteger (toInteger (other s))
    , scalarSource ("factory/" ++ label ++ "/bottom-int") (const (sourceInt undefined)) $ \_ -> do
        x <- M.lazy (error "factory forced ignored Int argument")
        callInt x
    , scalarSource ("factory/" ++ label ++ "/bottom-integer") (const (sourceInteger undefined)) $ \_ -> do
        x <- M.lazy (error "factory forced ignored Integer argument")
        callInteger x
    , scalarSource ("factory/" ++ label ++ "/repeated-int")
        (\s -> repeats s * sourceInt (number s)) $ \s -> do
        x <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (callInt x)
    , scalarSource ("factory/" ++ label ++ "/repeated-integer")
        (\s -> repeats s * sourceInteger (toInteger (other s))) $ \s -> do
        x <- M.lazy (pure (toInteger (other s)))
        sum <$> replicateM (repeats s) (callInteger x)
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
    int <- M.lazy (error "factory forced Int input" :: M.MemoristM Int)
    integerInput <- M.lazy (error "factory forced Integer input" :: M.MemoristM Integer)
    let ints = sum <$> sequence
            [caseFactoryM int, guardFactoryM int, guardFactoryRenamedM int]
        integers = sum <$> sequence
            [caseFactoryM integerInput, guardFactoryM integerInput, guardFactoryRenamedM integerInput]
        one = if even (variant s)
            then (+) <$> ints <*> integers
            else (+) <$> integers <*> ints
    sum <$> replicateM (repeats s) one
