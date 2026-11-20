module Cost.Translate.Frontend.LocalValues
    ( localSignatures
    , bodyTypeRenamings
    , withLocalThunk
    , allocateSpecialized
    , allocateShared
    , nativeAlias
    , usedNames
    , typedDeclarations
    , localFunctions
    , checkLocalBinding
    , checkLocalAliases
    , checkRecursiveGroup
    ) where

import Control.Monad (unless)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Frontend.Allocation (freshPrimed)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Thunks (reuseThunk)
import Cost.Translate.Frontend.Gates
import Cost.Translate.Frontend.NamePlan (NamePlan (..), plannedTemporaryNames)
import Cost.Translate.Frontend.Resolve (remainingArity)
import Cost.Translate.Frontend.Signatures
import Cost.Translate.Frontend.Strict (strictBindNames)
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing
    ( Specialization (..)
    , constrainedVariables
    , nativeSharing
    , renameAllocation
    , vacuousSharing
    )
import Cost.Translate.Sharing.Specialization (closedScalar)
import Cost.Translate.Typed (hasFreeVariables)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set qualified as Set
import GHC
import GHC.Core.TyCo.FVs (tyCoVarsOfTypes)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type
    ( isPredTy
    , splitForAllTyVars
    , splitFunTys
    )
import GHC.Data.Bag (bagToList)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (isTvOcc, occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

localSignatures :: Env -> HsValBinds GhcRn -> Either String (Map.Map Name O.Ty)
localSignatures env (XValBindsLR (NValBinds groups sigs)) = do
    let explicit =
            Set.fromList
                [unLoc n | L _ (TypeSig _ ns _) <- sigs, n <- ns]
        sigSpans =
            Map.fromList
                [(unLoc n, locA loc) | L loc (TypeSig _ ns _) <- sigs, n <- ns]
    signatures <-
        sequence
            [ (,) (unLoc name)
                <$> locatedAt
                    (Map.findWithDefault (getLocA name) (unLoc name) sigSpans)
                    ( translatedSignatureUsing
                        (unLoc name `Set.member` reflectiveBindings env)
                        factory
                        (sourceDatatypes env)
                        (arity matches)
                        ty
                    )
            | L _ (FunBind _ name matches) <- concatMap (bagToList . snd) groups
            , unLoc name `Set.member` explicit
            , Just ty <- [Map.lookup (unLoc name) (inferredBindings env)]
            , let factory =
                    arity matches == 0
                        && not (null (constrainedVariables ty))
            ]
    pure (Map.fromList signatures)
localSignatures _ _ = Right Map.empty

bodyTypeRenamings ::
    Map.Map Name O.Ty -> Env -> Name -> O.Expr -> Type -> Map.Map Name String
bodyTypeRenamings sigs scope name action ty
    | name `Map.member` sigs = Map.empty
    | otherwise =
        Map.fromList (snd (mapAccumL fresh used (fst (splitForAllTyVars ty))))
    where
        types = Map.elems (inferredBindings scope)
        expressionTypes = map snd (Map.elems (inferredExpressions scope))
        variables =
            concatMap (fst . splitForAllTyVars) types
                ++ nonDetEltsUniqSet (tyCoVarsOfTypes types)
        used =
            Set.fromList
                ( map getOccString variables
                    ++ [ occNameString (rdrNameOcc occurrence)
                       | occurrence <-
                            collect
                                (action, Map.elems sigs, expressionTypes) ::
                                [RdrName]
                       , isTvOcc (rdrNameOcc occurrence)
                       ]
                )
        fresh occupied variable =
            let occurrence = freshPrimed occupied (getOccString variable)
             in (Set.insert occurrence occupied, (getName variable, occurrence))

strictLocalValue :: Env -> LHsBind GhcRn -> Name -> Maybe Type -> Bool
strictLocalValue scope binding name bindingType =
    name `elem` strictBindNames [binding]
        && name `Set.notMember` strictWhnfValues scope
        && case bindingType of
            Just ty ->
                not
                    ( any
                        (isPredTy . scaledThing)
                        (fst (splitFunTys (snd (splitForAllTyVars ty))))
                    )
            Nothing -> False

withLocalThunk ::
    Env ->
    LHsBind GhcRn ->
    Name ->
    Maybe Type ->
    O.Expr ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
withLocalThunk scope binding name bindingType reference done
    | strictLocalValue scope binding name bindingType = do
        value <-
            maybe
                (Left "missing strict local name")
                Right
                (Map.lookup name (plannedStrictValues (namePlan scope)))
        let inner = case bindingType of
                Just ty
                    | closedScalar ty ->
                        rememberScalar (name, ty) (O.v value) bound
                _ -> bound
        continuation <- done inner
        pure
            ( O.doE
                [O.bind value (O.call "M.force" [reference])]
                (O.call "Prelude.seq" [O.v value, continuation])
            )
    | otherwise = done bound
    where
        bound = insertEnv name (Thunk reference) scope

usedNames :: O.Expr -> Set.Set String
usedNames continuation =
    Set.fromList
        (map (occNameString . rdrNameOcc) (collect continuation :: [RdrName]))

allocateSpecialized ::
    Map.Map Name O.Ty ->
    Env ->
    LocatedN Name ->
    O.Expr ->
    Maybe Type ->
    [(Specialization, String)] ->
    O.Expr ->
    Either String O.Expr
allocateSpecialized sigs scope name action bindingType instances continuation =
    let used = usedNames continuation
        needed = filter ((`Set.member` used) . snd) instances
     in if null needed
            then pure continuation
            else do
                ty <-
                    maybe
                        (Left "missing specialized local binding type")
                        Right
                        bindingType
                let source = unLoc name
                    renamings = bodyTypeRenamings sigs scope source action ty
                bodyType <-
                    locatedAt
                        (getLocA name)
                        ( translatedSignatureAsUsing
                            renamings
                            (unLoc name `Set.member` reflectiveBindings scope)
                            monadicType
                            (sourceDatatypes scope)
                            0
                            ty
                        )
                let bodyName = plannedBody scope (unLoc name)
                    body = [O.sig bodyName bodyType, O.val bodyName [] action]
                allocated <-
                    specializedAllocations
                        scope
                        source
                        bodyName
                        needed
                        continuation
                pure (O.letE body allocated)

specializedAllocations ::
    Env ->
    Name ->
    String ->
    [(Specialization, String)] ->
    O.Expr ->
    Either String O.Expr
specializedAllocations scope source bodyName instances continuation = do
    allocations <- mapM allocate named
    pure
        ( O.doE
            (map fst allocations)
            (O.letE (concatMap snd allocations) continuation)
        )
    where
        plan = namePlan scope
        occupied =
            usedNames continuation
                `Set.union` Set.fromList reserved
        reserved = Map.elems (plannedLocals plan) ++ plannedTemporaryNames plan
        named = snd (mapAccumL fresh occupied instances)
        fresh used (specialization, target) =
            let root = freshPrimed used (target ++ "anchor")
             in (Set.insert root used, (specialization, target, root))
        allocate = allocateSpecialization scope source bodyName

allocateSpecialization ::
    Env ->
    Name ->
    String ->
    (Specialization, String, String) ->
    Either String (O.Stmt, [O.Decl])
allocateSpecialization scope source bodyName (specialization, target, root) = do
    let ty = allocationType specialization
        names = bodyTypeRenamings Map.empty scope source (O.v bodyName) ty
        renamed = renameAllocation names ty
        datatypes = sourceDatatypes scope
        schemas = plannedNativeSchemas (namePlan scope)
    native <- nativeSharing schemas Map.empty renamed
    case native of
        Nothing -> do
            statement <- specializedAllocation scope bodyName (ty, target)
            pure (statement, [])
        Just (producerType, convert) -> do
            let action = O.v bodyName
                alias =
                    if vacuousSharing ty
                        then O.v root
                        else O.call "mapThunk" [convert, O.v root]
                allocate producer signatures =
                    ( O.bind root (lazyAction producer)
                    , signatures ++ [O.val target [] alias]
                    )
            if inferKeyFromUses specialization
                then pure (allocate action [])
                else do
                    signature <-
                        translatedSignatureAs
                            False
                            (O.tc "M.Th" . (: []))
                            datatypes
                            0
                            renamed
                    let producer = O.ann action producerType
                    pure (allocate producer [O.sig target signature])

specializedAllocation :: Env -> String -> (Type, String) -> Either String O.Stmt
specializedAllocation scope bodyName (instanceType, target) = do
    producer <-
        if hasFreeVariables instanceType
            then pure (O.v bodyName)
            else do
                rendered <- renderType (sourceDatatypes scope) instanceType
                pure (O.ann (O.v bodyName) (monadicType rendered))
    pure (O.bind target (lazyAction producer))

plannedBody :: Env -> Name -> String
plannedBody scope name =
    Map.findWithDefault
        (localName scope name ++ "body")
        name
        (plannedBodies (namePlan scope))

nativeAlias :: Env -> Maybe Type -> Match GhcRn (LHsExpr GhcRn) -> Maybe O.Expr
nativeAlias scope bindingType match = do
    ty <- bindingType
    let schemas = plannedNativeSchemas (namePlan scope)
    Right (Just _) <- pure (nativeSharing schemas Map.empty ty)
    case (grhssLocalBinds rhs, grhssGRHSs rhs) of
        (EmptyLocalBinds _, [L _ (GRHS _ [] expression)]) ->
            reuseThunk scope expression
        _ -> Nothing
    where
        rhs = m_grhss match

allocateShared ::
    Map.Map Name O.Ty ->
    Env ->
    LocatedN Name ->
    O.Expr ->
    Maybe Type ->
    Maybe O.Ty ->
    O.Expr ->
    Either String O.Expr
allocateShared sigs scope name action bindingType paths continuation
    | n `Set.notMember` usedNames continuation = pure continuation
    | otherwise = do
        native <- locatedAt (getLocA name) $ case bindingType of
            Nothing -> Right Nothing
            Just ty ->
                nativeSharing
                    (plannedNativeSchemas (namePlan scope))
                    (bodyTypeRenamings sigs scope (unLoc name) action ty)
                    ty
        let signature = maybeToList (O.sig n <$> Map.lookup (unLoc name) sigs)
            anchorVar =
                Map.findWithDefault
                    (n ++ "anchor")
                    (unLoc name)
                    (plannedAnchors (namePlan scope))
        pure $ case native of
            Just (bodyType, convert) ->
                let bodyName = plannedBody scope (unLoc name)
                    body = [O.sig bodyName bodyType, O.val bodyName [] action]
                    mapped =
                        if maybe False vacuousSharing bindingType
                            then O.v anchorVar
                            else O.call "mapThunk" [convert, O.v anchorVar]
                    shared = O.val n [] mapped
                 in O.letE
                        body
                        ( O.doE
                            [O.bind anchorVar (lazyAction (O.v bodyName))]
                            (O.letE (signature ++ [shared]) continuation)
                        )
            Nothing ->
                sharedAllocation
                    n
                    anchorVar
                    action
                    bindingType
                    paths
                    signature
                    continuation
    where
        n = localName scope (unLoc name)

sharedAllocation ::
    String ->
    String ->
    O.Expr ->
    Maybe Type ->
    Maybe O.Ty ->
    [O.Decl] ->
    O.Expr ->
    O.Expr
sharedAllocation n anchorName action bindingType paths signature continuation =
    let allocate =
            maybe
                (O.v "sharedAt")
                (\path -> O.call "sharedAtBy" [proxy path])
                paths
        parameters = [O.conP "()" [] | isJust paths]
        monomorphic = maybe False (null . fst . splitForAllTyVars) bindingType
        allocation =
            lazyAction $
                if monomorphic then action else O.ret (O.tuple [])
        declaration =
            O.val
                n
                parameters
                ( if monomorphic
                    then O.v anchorName
                    else O.app allocate [lazyAction action, O.v anchorName]
                )
     in O.doE
            [O.bind anchorName allocation]
            (O.letE (signature ++ [declaration]) continuation)
    where
        proxy path = O.ann (O.v "Proxy") (O.tc "Proxy" [path])

lazyAction :: O.Expr -> O.Expr
lazyAction action = O.call "M.lazy" [action]

typedDeclarations :: Map.Map Name O.Ty -> Env -> Name -> O.Decl -> [O.Decl]
typedDeclarations signatures scope name declaration =
    maybeToList (O.sig (localName scope name) <$> Map.lookup name signatures)
        ++ [declaration]

localFunctions :: Env -> [LHsBind GhcRn] -> [(Name, Binding)]
localFunctions scope bindings =
    [ (unLoc n, Function (localName scope (unLoc n)) (arity ms))
    | L _ (FunBind _ n ms) <- bindings
    , arity ms > 0
    ]

checkLocalBinding :: LHsBind GhcRn -> Either String ()
checkLocalBinding (L _ FunBind {}) = Right ()
checkLocalBinding binding = bindingError binding

checkLocalAliases :: Env -> Match GhcRn (LHsExpr GhcRn) -> Either String ()
checkLocalAliases scope match = mapM_ check (grhssGRHSs (m_grhss match))
    where
        check :: LGRHS GhcRn (LHsExpr GhcRn) -> Either String ()
        check (L _ (GRHS _ _ rhs)) = case remainingArity scope rhs of
            Right n
                | n > 0 ->
                    locatedAt
                        (getLocA rhs)
                        ( Left
                            "function-valued alias has no validated\
                            \ calling convention"
                        )
            _ -> Right ()

checkRecursiveGroup :: [(Name, Binding)] -> [LHsBind GhcRn] -> Either String ()
checkRecursiveGroup functions bindings =
    unless
        (length functions == length bindings)
        (Left (location ++ ": recursive value bindings are unsupported"))
    where
        location = case bindings of
            b : _ -> showSDocUnsafe (ppr (getLocA b))
            [] -> "empty binding group"
