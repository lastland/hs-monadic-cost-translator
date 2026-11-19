module Cost.Translate.Specialize.Analysis where

import Cost.Translate.Callable (expressions, spine)
import Cost.Translate.Typed
    ( FunctionType (..)
    , SourceKey
    , expressionTypeAt
    , sourceKey
    )
import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict qualified as Map
    ( Map
    , fromList
    , fromListWith
    , lookup
    )
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
    ( Set
    , fromList
    , member
    , notMember
    )
import GHC
    ( GenLocated (L)
    , GhcRn
    , HsBindLR (FunBind)
    , HsExpr (HsApp, HsVar, OpApp)
    , HsGroup (hs_valds)
    , HsValBinds
    , HsValBindsLR (ValBinds, XValBindsLR)
    , LHsBind
    , LHsExpr
    , Match (m_pats)
    , MatchGroup (mg_alts)
    , NHsValBindsLR (NValBinds)
    , Name
    , SrcSpan
    , TyVar
    , Type
    , getLocA
    , unLoc
    )
import GHC.Data.Bag (bagToList)
import GHC.Types.Basic (RecFlag (..))

data HelperReference
    = BareReference
    | PrefixApplication Int
    | InfixApplication Int

data HelperSpecialization = HelperSpecialization
    { specializedHelper :: Name
    , helperOwnedVariable :: TyVar
    , helperInstantiation :: Type
    , helperQualifiedType :: Type
    }

data LexicalOwner = LexicalOwner
    { ownerName :: Name
    , ownerSpan :: SrcSpan
    }

data ReferenceOwnership = ReferenceOwnership
    { referenceAncestors :: [LexicalOwner]
    , referenceSpan :: SrcSpan
    }

type ReferenceSite = (Name, SourceKey)

data Analysis = Analysis
    { bindings :: [LHsBind GhcRn]
    , recursive :: Set.Set Name
    , candidates :: [(Name, Int, FunctionType)]
    , declarations :: Map.Map Name [TyVar]
    , lexical :: Map.Map Name [Name]
    , uses :: Map.Map Name [(SourceKey, Maybe Type)]
    , ownership :: Map.Map ReferenceSite [ReferenceOwnership]
    , calls :: Map.Map ReferenceSite [HelperReference]
    , checked :: Map.Map Name FunctionType
    }

analyze ::
    HsGroup GhcRn ->
    Map.Map Name FunctionType ->
    Map.Map SourceKey Type ->
    Analysis
analyze group checked hints =
    Analysis
        { bindings = collect group
        , recursive = recursiveNames group
        , candidates = helperCandidates group checked
        , declarations = Map.fromList (scopes checked [] group)
        , lexical = Map.fromList (paths [] group)
        , uses = helperUses group hints
        , ownership = Map.fromListWith (++) (referenceOwners [] group)
        , calls = helperCalls group
        , checked = checked
        }

topLevelNames :: HsGroup GhcRn -> Set.Set Name
topLevelNames group =
    Set.fromList
        [ unLoc name
        | L _ (FunBind _ name _) <- case hs_valds group of
            XValBindsLR (NValBinds groups _) ->
                concatMap (bagToList . snd) groups
            ValBinds _ binds _ -> bagToList binds
        ]
recursiveNames :: HsGroup GhcRn -> Set.Set Name
recursiveNames group =
    Set.fromList
        [ unLoc name
        | XValBindsLR (NValBinds groups _) <-
            collect group :: [HsValBinds GhcRn]
        , (Recursive, binds) <- groups
        , L _ (FunBind _ name _) <- bagToList binds
        ]
singletonRecursiveNames :: HsGroup GhcRn -> Set.Set Name
singletonRecursiveNames group =
    Set.fromList
        [ unLoc name
        | XValBindsLR (NValBinds groups _) <-
            collect group :: [HsValBinds GhcRn]
        , (Recursive, binds) <- groups
        , [L _ (FunBind _ name matches)] <- [bagToList binds]
        , arity matches > 0
        ]

helperCandidates ::
    HsGroup GhcRn ->
    Map.Map Name FunctionType ->
    [(Name, Int, FunctionType)]
helperCandidates group checked =
    [ (unLoc name, arity matches, info)
    | L _ (FunBind _ name matches) <- collect group :: [LHsBind GhcRn]
    , arity matches > 0
    , unLoc name `Set.notMember` topLevel
    , unLoc name `Set.notMember` recursive
        || unLoc name `Set.member` singletonRecursive
    , Just info <- [Map.lookup (unLoc name) checked]
    ]
    where
        topLevel = topLevelNames group
        recursive = recursiveNames group
        singletonRecursive = singletonRecursiveNames group

paths :: (Data a) => [Name] -> a -> [(Name, [Name])]
paths ancestors syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
    Just binding@(L _ (FunBind _ name _)) ->
        (unLoc name, ancestors)
            : concat (gmapQ (paths (ancestors ++ [unLoc name])) (unLoc binding))
    _ -> concat (gmapQ (paths ancestors) syntax)
scopes ::
    (Data a) => Map.Map Name FunctionType -> [TyVar] -> a -> [(Name, [TyVar])]
scopes checked variables syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
    Just binding@(L _ (FunBind _ name matches)) ->
        let owned =
                [ variable
                | arity matches > 0
                , Just info <- [Map.lookup (unLoc name) checked]
                , variable <- functionVariables info
                ]
            nested = variables ++ owned
         in (unLoc name, variables)
                : concat (gmapQ (scopes checked nested) (unLoc binding))
    _ -> concat (gmapQ (scopes checked variables) syntax)

helperUses ::
    HsGroup GhcRn ->
    Map.Map SourceKey Type ->
    Map.Map Name [(SourceKey, Maybe Type)]
helperUses group hints =
    Map.fromListWith
        (++)
        [ ( unLoc name
          , [(location expression, useType expression)]
          )
        | expression@(L _ (HsVar _ name)) <- source
        ]
    where
        source = collect group :: [LHsExpr GhcRn]
        useType = expressionTypeAt source hints

referenceOwners ::
    (Data a) => [LexicalOwner] -> a -> [(ReferenceSite, [ReferenceOwnership])]
referenceOwners owners syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
    Just binding@(L _ (FunBind _ name _)) ->
        let owner =
                LexicalOwner
                    { ownerName = unLoc name
                    , ownerSpan = getLocA binding
                    }
         in concat (gmapQ (referenceOwners (owners ++ [owner])) (unLoc binding))
    _ -> case cast syntax :: Maybe (LHsExpr GhcRn) of
        Just expression@(L _ (HsVar _ name)) ->
            [
                ( (unLoc name, location expression)
                ,
                    [ ReferenceOwnership
                        { referenceAncestors = owners
                        , referenceSpan = getLocA expression
                        }
                    ]
                )
            ]
        _ -> concat (gmapQ (referenceOwners owners) syntax)

helperApplications ::
    HsGroup GhcRn ->
    [(ReferenceSite, HelperReference)]
helperApplications group =
    [ ((unLoc name, location function), reference (length arguments))
    | expression <- expressions group
    , (function, arguments, reference) <- case unLoc expression of
        HsApp {} ->
            let (function, arguments) = spine expression
             in [(function, arguments, PrefixApplication)]
        OpApp _ left operator right ->
            [(operator, [left, right], InfixApplication)]
        _ -> []
    , HsVar _ name <- [unLoc function]
    ]

helperCalls :: HsGroup GhcRn -> Map.Map ReferenceSite [HelperReference]
helperCalls group =
    Map.fromListWith
        (++)
        ( [(site, [application]) | (site, application) <- applications]
            ++ [ (site, [BareReference])
               | expression@(L _ (HsVar _ name)) <- expressions group
               , let site = (unLoc name, location expression)
               , site `Set.notMember` heads
               ]
        )
    where
        applications = helperApplications group
        heads = Set.fromList (map fst applications)

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = case unLoc (mg_alts matches) of
    L _ match : _ -> length (m_pats match)
    [] -> 0

location :: LHsExpr GhcRn -> SourceKey
location = sourceKey . getLocA

collect :: (Data a, Data b) => b -> [a]
collect value = maybeToList (cast value) ++ concat (gmapQ collect value)
