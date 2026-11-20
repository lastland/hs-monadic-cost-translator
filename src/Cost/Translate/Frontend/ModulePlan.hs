module Cost.Translate.Frontend.ModulePlan
    ( moduleEnvironment
    , topLevelGroups
    , reservedModuleNames
    , constructorLayouts
    , bindingSignature
    ) where

import Cost.Translate.Callable (expressions)
import Cost.Translate.Datatypes (constructors, renderType, selectors)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Gates (bindingError, locatedAt)
import Cost.Translate.Frontend.NamePlan (planNames)
import Cost.Translate.Frontend.Signatures (monadicType, translatedSignatureAs)
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing
    ( AllocationScope (..)
    , Specialization
    , constrainedVariables
    , planSpecializations
    )
import Cost.Translate.Typed qualified as Typed
import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Data.Bag (bagToList)
import GHC.Types.Basic (RecFlag (..))
import GHC.Types.Name (getOccString)

moduleEnvironment ::
    Set.Set Name ->
    [TyCon] ->
    Map.Map Typed.SourceKey String ->
    Map.Map Name Type ->
    Map.Map Typed.SourceKey Type ->
    Map.Map Name AllocationScope ->
    HsGroup GhcRn ->
    (Env, Map.Map Name [Specialization])
moduleEnvironment strict datatypes literals types hints checkedScopes group =
    let binds = concatMap snd (topLevelGroups group)
        topNames = Set.fromList [unLoc n | L _ (FunBind _ n _) <- binds]
        specialized =
            specializedLocals
                datatypes
                types
                hints
                checkedScopes
                topNames
                group
        reflected = reflectedLocals types topNames specialized group
        expressionTypes =
            Map.fromList
                [ (location, (ty, rendered))
                | (location, ty) <- Map.toList hints
                , Right rendered <- [renderType datatypes ty]
                ]
        env =
            Env
                ( Map.fromList
                    ( [ (n, Constructor c a fs)
                      | (n, c, a, fs) <- constructors datatypes
                      ]
                        ++ [(n, Function f 1) | (n, f) <- selectors datatypes]
                    )
                )
                literals
                types
                expressionTypes
                datatypes
                reflected
                ( Set.fromList
                    ( "M.PairT"
                        : [ generatedName "T" (getName c)
                          | tc <- datatypes
                          , [c] <- [tyConDataCons tc]
                          ]
                    )
                )
                (planNames datatypes globals layouts specialized group)
                strict
                (explicitScopes group)
                Map.empty
     in (env, specialized)
    where
        globals = reservedModuleNames datatypes group
        layouts = constructorLayouts datatypes

topLevelGroups :: HsGroup GhcRn -> [(RecFlag, [LHsBind GhcRn])]
topLevelGroups group = case hs_valds group of
    XValBindsLR (NValBinds groups _) ->
        [(recursive, bagToList bindings) | (recursive, bindings) <- groups]
    ValBinds _ bs _ -> [(NonRecursive, bagToList bs)]

reservedModuleNames :: [TyCon] -> HsGroup GhcRn -> Set.Set String
reservedModuleNames datatypes group =
    Set.fromList
        ( [ generatedName "M" (unLoc n)
          | L _ (FunBind _ n _) <- concatMap snd (topLevelGroups group)
          ]
            ++ map snd (selectors datatypes)
        )

constructorLayouts :: [TyCon] -> Map.Map Name [Name]
constructorLayouts datatypes =
    Map.fromList [(n, fields) | (n, _, _, fields) <- constructors datatypes]

specializedLocals ::
    [TyCon] ->
    Map.Map Name Type ->
    Map.Map Typed.SourceKey Type ->
    Map.Map Name AllocationScope ->
    Set.Set Name ->
    HsGroup GhcRn ->
    Map.Map Name [Specialization]
specializedLocals datatypes types hints checkedScopes topNames group =
    Map.fromList
        [ (unLoc name, instantiations)
        | L _ (FunBind _ name matches) <- collect group :: [LHsBind GhcRn]
        , arity matches == 0
        , unLoc name `Set.notMember` topNames
        , Just ty <- [Map.lookup (unLoc name) types]
        , Just instantiations <-
            [ planSpecializations
                datatypes
                (Map.findWithDefault Nothing (unLoc name) allocationEvidence)
                ty
                (Map.findWithDefault [] (unLoc name) useTypes)
            ]
        ]
    where
        sourceExpressions = collect group :: [LHsExpr GhcRn]
        useType = Typed.expressionTypeAt sourceExpressions hints
        useTypes =
            Map.fromListWith
                (++)
                [ (unLoc name, [useType expression])
                | expression@(L _ (HsVar _ name)) <- sourceExpressions
                ]
        allocationEvidence = Map.fromList (allocationScopes owners root group)
        root = Just (AllocationScope [] [] (Just []))
        owners = Map.mapWithKey named checkedScopes
        signatures = Set.union topNames (Set.fromList signed)
        signed =
            [unLoc n | TypeSig _ ns _ <- collect group :: [Sig GhcRn], n <- ns]
        named owner scope
            | null (ownerVariables scope) = scope {namedContext = Just []}
            | owner `Set.member` signatures = scope
            | otherwise = scope {namedContext = Nothing}

allocationScopes ::
    (Data a) =>
    Map.Map Name AllocationScope ->
    Maybe AllocationScope ->
    a ->
    [(Name, Maybe AllocationScope)]
allocationScopes known scope value = case source of
    Just (L _ (FunBind _ name matches)) ->
        let nested = do
                AllocationScope vars givens outer <- scope
                AllocationScope vs ps inner <- Map.lookup (unLoc name) known
                let variables = vars ++ vs
                    names = Set.fromList (map getOccString variables)
                    visible
                        | Set.size names == length variables =
                            (++) <$> outer <*> inner
                        | otherwise = Nothing
                pure (AllocationScope variables (givens ++ ps) visible)
         in (unLoc name, scope)
                : allocationScopes known nested matches
    _ -> concat (gmapQ (allocationScopes known scope) value)
    where
        source = cast value :: Maybe (LHsBind GhcRn)

reflectedLocals ::
    Map.Map Name Type ->
    Set.Set Name ->
    Map.Map Name [Specialization] ->
    HsGroup GhcRn ->
    Set.Set Name
reflectedLocals types topNames specialized group =
    closure
        ( Set.fromList
            [ unLoc n
            | L _ (FunBind _ n ms) <- sourceBinds
            , arity ms == 0
            , unLoc n `Set.notMember` topNames
            , unLoc n `Map.notMember` specialized
            , Just ty <- [Map.lookup (unLoc n) types]
            , not (null (constrainedVariables ty))
            ]
        )
    where
        sourceBinds = collect group :: [LHsBind GhcRn]
        closure known =
            let next =
                    known
                        `Set.union` Set.fromList
                            [ unLoc n
                            | b@(L _ (FunBind _ n _)) <- sourceBinds
                            , any
                                (`Set.member` known)
                                [unLoc v | L _ (HsVar _ v) <- expressions b]
                            ]
             in if next == known then known else closure next

bindingSignature :: Env -> LHsBind GhcRn -> Either String (String, O.Ty)
bindingSignature env (L _ (FunBind _ name matches)) = do
    ty <-
        maybe
            (Left "missing inferred binding type")
            Right
            (Map.lookup (unLoc name) (inferredBindings env))
    signature <-
        locatedAt
            (getLocA name)
            ( translatedSignatureAs
                (unLoc name `Set.member` reflectiveBindings env)
                monadicType
                (sourceDatatypes env)
                (arity matches)
                ty
            )
    pure (generatedName "M" (unLoc name), signature)
bindingSignature _ b = bindingError b
