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
import Cost.Translate.Sharing (constrainedVariables, specializedSharing)
import Cost.Translate.Typed qualified as Typed
import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Data.Bag (bagToList)
import GHC.Types.Basic (RecFlag (..))

moduleEnvironment ::
    Set.Set Name ->
    [TyCon] ->
    Map.Map String String ->
    Map.Map Name Type ->
    Map.Map String Type ->
    Map.Map Name ([TyVar], [Type]) ->
    HsGroup GhcRn ->
    (Env, Map.Map Name [Type])
moduleEnvironment strict datatypes literals types hints checkedScopes group =
    let binds = concatMap snd (topLevelGroups group)
        topNames =
            Set.fromList
                [unLoc n | L _ (FunBind _ n _) <- binds]
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
                ( planNames
                    (reservedModuleNames datatypes group)
                    (constructorLayouts datatypes)
                    specialized
                    group
                )
                strict
     in (env, specialized)

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
    Map.Map String Type ->
    Map.Map Name ([TyVar], [Type]) ->
    Set.Set Name ->
    HsGroup GhcRn ->
    Map.Map Name [Type]
specializedLocals datatypes types hints checkedScopes topNames group =
    Map.fromList
        [ (unLoc name, instantiations)
        | L _ (FunBind _ name matches) <- collect group :: [LHsBind GhcRn]
        , arity matches == 0
        , unLoc name `Set.notMember` topNames
        , let (variables, givens) =
                Map.findWithDefault ([], []) (unLoc name) allocationEvidence
        , Just ty <- [Map.lookup (unLoc name) types]
        , Just instantiations <-
            [ specializedSharing
                datatypes
                variables
                givens
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
        allocationEvidence =
            Map.fromList $
                allocationScopes checkedScopes ([], []) group

allocationScopes ::
    (Data a) =>
    Map.Map Name ([TyVar], [Type]) ->
    ([TyVar], [Type]) ->
    a ->
    [(Name, ([TyVar], [Type]))]
allocationScopes known scope@(vars, givens) value = case source of
    Just binding@(L _ (FunBind _ name matches)) ->
        let (scopedVariables, scopedGivens) =
                if arity matches > 0
                    then Map.findWithDefault ([], []) (unLoc name) known
                    else ([], [])
            nested = (vars ++ scopedVariables, givens ++ scopedGivens)
            descend :: (Data b) => b -> [(Name, ([TyVar], [Type]))]
            descend = allocationScopes known nested
         in (unLoc name, scope)
                : concat
                    (gmapQ descend (unLoc binding))
    _ -> concat (gmapQ (allocationScopes known scope) value)
    where
        source = cast value :: Maybe (LHsBind GhcRn)

reflectedLocals ::
    Map.Map Name Type ->
    Set.Set Name ->
    Map.Map Name [Type] ->
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
