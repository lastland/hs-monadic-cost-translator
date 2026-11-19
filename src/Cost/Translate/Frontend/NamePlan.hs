module Cost.Translate.Frontend.NamePlan
    ( NamePlan (..)
    , planNames
    , plannedTemporaryNames
    ) where

import Cost.Translate.Callable (expressions, spine)
import Cost.Translate.Frontend.Allocation
import Cost.Translate.Frontend.Match.Paths
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Sharing (Specialization)
import Cost.Translate.Sharing.Schema
import Data.Data (Data)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC

data NamePlan = NamePlan
    { plannedLocals :: Map.Map Name String
    , plannedParameters :: Map.Map (Name, Int) String
    , plannedArguments :: Map.Map String [String]
    , plannedActions :: Map.Map String String
    , plannedFields :: Map.Map String String
    , plannedMatches :: Map.Map String String
    , plannedAnchors :: Map.Map Name String
    , plannedBodies :: Map.Map Name String
    , plannedSpecializations :: Map.Map Name [(Specialization, String)]
    , plannedNativeSchemas :: NativeSchemas
    }

plannedTemporaryNames :: NamePlan -> [String]
plannedTemporaryNames plan =
    Set.toList (nativeSchemaNames (plannedNativeSchemas plan))
        ++ concat (Map.elems (plannedArguments plan))
        ++ Map.elems (plannedActions plan)
        ++ Map.elems (plannedFields plan)
        ++ Map.elems (plannedMatches plan)
        ++ Map.elems (plannedAnchors plan)
        ++ Map.elems (plannedBodies plan)
        ++ concatMap (map snd) (Map.elems (plannedSpecializations plan))

planNames ::
    (Data a) =>
    [TyCon] ->
    Set.Set String ->
    Map.Map Name [Name] ->
    Map.Map Name [Specialization] ->
    a ->
    NamePlan
planNames datatypes globalNames layouts specialized syntax =
    let binds = collect syntax :: [LHsBind GhcRn]
        schemas = planNativeSchemas globalNames datatypes
        schemaNames = nativeSchemaNames schemas
        reserved =
            Set.unions [globalNames, fixedReservedNames, schemaNames]
        binders = sourceBinders syntax binds
        (locals, afterLocals) = assignBinders reserved binders
        (parameters, afterParameters) =
            assignParameters locals afterLocals (concatMap parametersFor binds)
        sites = concatMap (expressionSites layouts) (expressions syntax)
        (arguments, afterArguments) = assignArguments afterParameters sites
        (actions, afterActions) =
            assignActions afterArguments Map.empty Map.empty $
                sourceActions syntax
        roots = patternRoots syntax
        fieldKeys = nub (concatMap (uncurry (plannedFieldKeys layouts)) roots)
        (fields, afterFields) = assignFields afterActions fieldKeys
        matchKeys = nub (concatMap (uncurry (plannedMatchKeys layouts)) roots)
        (matches, afterMatches) = assignPrimed afterFields "matched" matchKeys
        names = [unLoc name | L _ (FunBind _ name _) <- binds]
        (anchors, afterAnchors) =
            assignBindingNames "anchor" locals afterMatches names
        (bodies, afterBodies) =
            assignBindingNames "body" locals afterAnchors names
        (specializations, _) =
            assignSpecializations
                locals
                afterBodies
                [ (name, types)
                | name <- names
                , Just types <- [Map.lookup name specialized]
                ]
     in NamePlan
            locals
            parameters
            arguments
            actions
            fields
            matches
            anchors
            bodies
            specializations
            schemas

sourceBinders :: (Data a) => a -> [LHsBind GhcRn] -> [Name]
sourceBinders syntax binds =
    nub
        ( [unLoc name | L _ (FunBind _ name _) <- binds]
            ++ [unLoc n | L _ (VarPat _ n) <- collect syntax :: [LPat GhcRn]]
            ++ [unLoc n | L _ (AsPat _ n _) <- collect syntax :: [LPat GhcRn]]
        )

parametersFor :: LHsBind GhcRn -> [((Name, Int), Maybe Name)]
parametersFor (L _ (FunBind _ name matches)) =
    [ ((unLoc name, i), binder)
    | (i, binder) <- zip [0 ..] (parameterBinders matches)
    ]
parametersFor _ = []

expressionSites :: Map.Map Name [Name] -> LHsExpr GhcRn -> [(String, Int)]
expressionSites layouts expression = case unLoc expression of
    HsApp {} -> [(key, length (snd (spine expression)))]
    OpApp {} -> [(key, 2)]
    ExplicitTuple _ arguments _ -> [(key, length arguments)]
    RecordCon _ name fields ->
        let count =
                maybe
                    (length (rec_flds fields))
                    length
                    (Map.lookup (unLoc name) layouts)
         in [(key, count)]
    ExplicitList _ elements ->
        [ (key ++ "_list" ++ show depth, 2)
        | depth <- [0 .. length elements - 1]
        ]
    _ -> []
    where
        key = actionKey "argument" (getLocA expression)

sourceActions :: (Data a) => a -> [String]
sourceActions syntax =
    nub
        ( concat
            [ map (actionKey "nextEquation" . getLocA) equations
            | matches <- collect syntax :: [MatchGroup GhcRn (LHsExpr GhcRn)]
            , let equations = withoutLast (unLoc (mg_alts matches))
            ]
            ++ [ actionKey "nextGuard" (getLocA guard)
               | guard@(L _ (GRHS _ guards _)) <- sourceGuards
               , not (null guards)
               ]
            ++ [ actionKey "caseThunk" (getLocA expression)
               | expression <- expressions syntax
               , HsCase {} <- [unLoc expression]
               ]
            ++ [ actionKey "guardThunk" (getLocA statement)
               | statement@(L _ BindStmt {}) <- statements
               ]
        )
    where
        sourceGuards = collect syntax :: [LGRHS GhcRn (LHsExpr GhcRn)]
        statements = collect syntax :: [GuardLStmt GhcRn]
        withoutLast [] = []
        withoutLast [_] = []
        withoutLast (item : items) = item : withoutLast items
