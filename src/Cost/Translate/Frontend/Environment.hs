module Cost.Translate.Frontend.Environment
    ( Binding (..)
    , Env (..)
    , lookupEnv
    , insertEnv
    , extendEnv
    , knownScalar
    , rememberScalar
    , aliasScalars
    , localName
    , parameterNames
    , occupiedNames
    , actionName
    , plannedAction
    , argumentNames
    , partialName
    ) where

import Cost.Translate.Frontend.Allocation
import Cost.Translate.Frontend.NamePlan
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Output qualified as O
import Cost.Translate.Typed (SourceKey)
import Data.Foldable (asum)
import Data.List (find, mapAccumL, partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Core.TyCo.Compare (eqType)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)

data Binding
    = Thunk O.Expr
    | SpecializedThunks [(Type, O.Expr)]
    | Function String Int
    | Adapter O.Expr Int
    | Constructor String Int [Name]

data Env = Env
    { envBindings :: Map.Map Name Binding
    , inferredLiterals :: Map.Map SourceKey String
    , inferredBindings :: Map.Map Name Type
    , inferredExpressions :: Map.Map SourceKey (Type, O.Ty)
    , scalarEvidence :: Map.Map SourceKey Type
    , knownScalarValues :: Map.Map Name [(Type, O.Expr)]
    , probingMatchExit :: Bool
    , scalarAliases :: [(Type, Set.Set Name)]
    , sourceDatatypes :: [TyCon]
    , reflectiveBindings :: Set.Set Name
    , singleConstructors :: Set.Set String
    , namePlan :: NamePlan
    , strictWhnfValues :: Set.Set Name
    , scopedTypeVariables :: Map.Map Name [Name]
    , producerCalls :: Map.Map Name String
    }

lookupEnv :: Name -> Env -> Maybe Binding
lookupEnv name = Map.lookup name . envBindings

insertEnv :: Name -> Binding -> Env -> Env
insertEnv name binding = extendEnv [(name, binding)]

extendEnv :: [(Name, Binding)] -> Env -> Env
extendEnv additions env =
    env
        { envBindings = Map.union bindings (envBindings env)
        , knownScalarValues = Map.difference (knownScalarValues env) bindings
        , scalarAliases =
            [ (ty, kept)
            | (ty, group) <- scalarAliases env
            , let kept = Set.difference group (Map.keysSet bindings)
            , Set.size kept > 1
            ]
        }
    where
        bindings = Map.fromList additions

knownScalar :: Env -> (Name, Type) -> Maybe O.Expr
knownScalar env (name, ty) = do
    entries <- Map.lookup name (knownScalarValues env)
    snd <$> find (eqType ty . fst) entries

rememberScalar :: (Name, Type) -> O.Expr -> Env -> Env
rememberScalar (name, ty) value env =
    env {knownScalarValues = Set.foldr store (knownScalarValues env) members}
    where
        members = case find contains (scalarAliases env) of
            Just (_, group) -> group
            Nothing -> Set.singleton name
        contains (t, group) = Set.member name group && eqType t ty
        store n = Map.insertWith update n [(ty, value)]
        update new old = new ++ filter (not . eqType ty . fst) old

aliasScalars :: [(Name, Type)] -> Env -> Env
aliasScalars [] env = env
aliasScalars ((name, ty) : rest) env
    | Set.size names < 2 = env
    | otherwise = case value of
        Just v -> rememberScalar (name, ty) v linked
        Nothing -> linked
    where
        names = Set.fromList (name : [n | (n, t) <- rest, eqType ty t])
        overlaps (t, group) = not (Set.disjoint names group) && eqType ty t
        (same, other) = partition overlaps (scalarAliases env)
        members = Set.unions (names : map snd same)
        linked = env {scalarAliases = (ty, members) : other}
        value = asum [knownScalar env (n, ty) | n <- Set.toList members]

localName :: Env -> Name -> String
localName env name = Map.findWithDefault "local" name locals
    where
        locals = plannedLocals (namePlan env)

parameterNames :: Env -> Name -> MatchGroup GhcRn (LHsExpr GhcRn) -> [String]
parameterNames env name matches =
    snd (mapAccumL assign (occupiedNames env <> localBindings) parameters)
    where
        parameters = zip [0 ..] (parameterBinders matches)
        localBindings =
            Set.fromList
                [ localName env (unLoc n)
                | L _ (FunBind _ n _) <- collect matches :: [LHsBind GhcRn]
                ]
        assign used (index, binder) =
            let hint = maybe (planned index) sourceName binder
                fresh = freshPrimed used hint
             in (Set.insert fresh used, fresh)
        sourceName n =
            let occurrence = getOccString n
             in if simpleIdentifier occurrence then occurrence else "local"
        planned i =
            Map.findWithDefault
                ("arg" ++ show i)
                (name, i)
                (plannedParameters (namePlan env))

occupiedNames :: Env -> Set.Set String
occupiedNames env =
    fixedReservedNames
        `Set.union` Set.fromList
            ( [ occNameString (rdrNameOcc name)
              | binding <- Map.elems (envBindings env)
              , expression <- case binding of
                    Thunk thunk -> [thunk]
                    SpecializedThunks thunks -> map snd thunks
                    _ -> []
              , name <- collect expression :: [RdrName]
              ]
                ++ [ name
                   | Function name _ <- Map.elems (envBindings env)
                   , simpleIdentifier name
                   ]
                ++ plannedTemporaryNames (namePlan env)
                ++ Map.elems (producerCalls env)
            )

actionName :: Env -> String -> SrcSpan -> String
actionName env prefix = plannedAction env . actionKey prefix

plannedAction :: Env -> String -> String
plannedAction env key = Map.findWithDefault key key actions
    where
        actions = plannedActions (namePlan env)

argumentNames :: Env -> String -> Int -> [String]
argumentNames env key count =
    Map.findWithDefault
        [key ++ "_" ++ show i | i <- [0 .. count - 1]]
        key
        (plannedArguments (namePlan env))

partialName :: Env -> Int -> String
partialName env target = choose target (0 :: Int)
    where
        used =
            Set.fromList
                ( Map.elems (plannedLocals plan)
                    ++ Map.elems (plannedParameters plan)
                    ++ plannedTemporaryNames plan
                )
        plan = namePlan env
        choose remaining count
            | candidate `Set.member` used = choose remaining (count + 1)
            | remaining == 0 = candidate
            | otherwise = choose (remaining - 1) (count + 1)
            where
                candidate = "partial" ++ replicate count '\''
