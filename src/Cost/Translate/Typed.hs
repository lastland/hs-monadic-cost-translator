module Cost.Translate.Typed
    ( SourceKey
    , sourceKey
    , expressionTypes
    , expressionTypeAt
    , FunctionType (..)
    , functionTypes
    , AllocationScope (..)
    , emptyAllocationScope
    , extendAllocationScope
    , functionGivens
    , scopedVariables
    , hasFreeVariables
    , hasFreshAnnotationVariables
    ) where

import Data.Data (Data, cast, gmapQ)
import Data.List (isSuffixOf, maximumBy, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Set qualified as Set
import GHC hiding (exprType)
import GHC.Core.ConLike (conLikeName)
import GHC.Core.FVs (exprFreeIds)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledMult, scaledThing)
import GHC.Core.TyCo.Subst (substTy)
import GHC.Core.Type (getTyVar_maybe, isPredTy, mkTyVarTy, mkVisFunTy)
import GHC.Core.Unify (tcMatchTy)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.Data.Strict qualified as Strict
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types (tcg_ev_binds)
import GHC.Tc.Types.Evidence
    ( EvBind (..)
    , EvTerm (..)
    , HsWrapper (..)
    , TcEvBinds (..)
    )
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Id (isDFunId)
import GHC.Types.Name (getOccString)
import GHC.Types.SrcLoc (BufSpan)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var (Var, isId)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

type SourceKey = Either String (RealSrcSpan, Strict.Maybe BufSpan)

sourceKey :: SrcSpan -> SourceKey
sourceKey (RealSrcSpan virtual buffer) = Right (virtual, buffer)
sourceKey span' = Left (showSDocUnsafe (ppr span'))

expressionTypes ::
    HscEnv ->
    TypecheckedModule ->
    [LHsExpr GhcRn] ->
    [LHsExpr GhcTc] ->
    IO (Map.Map SourceKey Type)
expressionTypes session checked source typed = do
    let locations =
            Map.fromListWith
                (+)
                [(location e, 1 :: Int) | e <- source, interesting (unLoc e)]
        original =
            Map.fromListWith
                (\_ outer -> outer)
                [ (location e, e)
                | e <- typed
                , Map.lookup (location e) locations == Just 1
                ]
        names =
            Map.fromListWith
                (++)
                [ (location e, [(unLoc name, getLocA e)])
                | e@(L _ (HsVar _ name)) <- source
                ]
        occurrences = Map.fromListWith (+) [(location e, 1 :: Int) | e <- typed]
        references =
            Map.fromListWith
                (++)
                [ (sourceKey (last path), [(name, path, e)])
                | e <- typed
                , Just (name, path) <- [reference e]
                ]
        selected = Map.mapMaybeWithKey choose original
        choose = recover names occurrences references
    (_, result) <-
        initDs session (fst (tm_internals_ checked)) $
            traverse (fmap exprType . dsLExpr) selected
    maybe (fail "GHC type-evidence desugaring failed") pure result
    where
        location = sourceKey . getLocA
        interesting HsVar {} = True
        interesting ExprWithTySig {} = True
        interesting _ = False

recover ::
    Map.Map SourceKey [(Name, SrcSpan)] ->
    Map.Map SourceKey Int ->
    Map.Map SourceKey [(Name, [SrcSpan], LHsExpr GhcTc)] ->
    SourceKey ->
    LHsExpr GhcTc ->
    Maybe (LHsExpr GhcTc)
recover names counts refs site original = case Map.lookup site names of
    Just [(name, sourceSpan)] -> do
        candidates <- Map.lookup site refs
        let pathLength (_, path, _) = length path
            (_, outerPath, outer) = maximumBy (comparing pathLength) candidates
            sameReference (checkedName, path, _) =
                checkedName == name && path `isSuffixOf` outerPath
            uniqueSpan span' =
                Map.lookup (sourceKey span') counts == Just 1
        if last outerPath == sourceSpan
            && all sameReference candidates
            && length candidates == length outerPath
            && all uniqueSpan outerPath
            then Just outer
            else Nothing
    Nothing
        | Map.lookup site counts == Just 1 -> Just original
    _ -> Nothing
reference :: LHsExpr GhcTc -> Maybe (Name, [SrcSpan])
reference expression = case getLocA expression of
    RealSrcSpan {} -> do
        (name, path) <- nested (unLoc expression)
        pure (name, getLocA expression : path)
    _ -> Nothing
nested :: HsExpr GhcTc -> Maybe (Name, [SrcSpan])
nested (HsVar _ name) = Just (getName (unLoc name), [])
nested (XExpr (ConLikeTc constructor _ _)) = Just (conLikeName constructor, [])
nested (HsPar _ expression) = reference expression
nested (XExpr (WrapExpr (HsWrap wrapper expression)))
    | applications wrapper = nested expression
nested _ = Nothing
applications :: HsWrapper -> Bool
applications (WpCompose left right) = applications left && applications right
applications WpTyApp {} = True
applications WpEvApp {} = True
applications WpHole = True
applications _ = False

expressionTypeAt ::
    [LHsExpr GhcRn] -> Map.Map SourceKey Type -> LHsExpr GhcRn -> Maybe Type
expressionTypeAt source hints = select
    where
        locations =
            Map.fromListWith
                (+)
                [(location e, 1 :: Int) | e <- source]
        select expression = case getLocA expression of
            RealSrcSpan {}
                | Map.lookup (location expression) locations == Just 1 ->
                    Map.lookup (location expression) hints
            _ -> Nothing
        location = sourceKey . getLocA

data FunctionType = FunctionType
    { functionDeclaredType :: Type
    , functionVariables :: [TyVar]
    , functionBodyType :: Type
    , functionEvidence :: ([Id], [EvBind])
    }

functionTypes :: TypecheckedModule -> Map.Map Name FunctionType
functionTypes checked =
    Map.mapMaybe unique $
        Map.fromListWith
            (++)
            [ ( getName (abe_poly export)
              ,
                  [ FunctionType
                        (idType (abe_poly export))
                        (variables abstraction export wrapper)
                        (bodyType (mg_ext matches))
                        (evidence <> wrapperEvidence wrapper)
                  ]
              )
            | (evidence, abstraction) <-
                lexicalAbstractions
                    ([], bagToList (tcg_ev_binds (fst (tm_internals_ checked))))
                    (tm_typechecked_source checked)
            , export <- abs_exports abstraction
            , L _ (FunBind (wrapper, _) identifier matches) <-
                bagToList (abs_binds abstraction)
            , unLoc identifier == abe_mono export
            ]
    where
        unique [function] = Just function
        unique _ = Nothing
        variables abstraction export wrapper
            | abs_sig abstraction = wrapperVariables wrapper
            | otherwise =
                [ variable
                | let free = tyCoVarsOfType (idType (abe_mono export))
                , variable <- abs_tvs abstraction
                , variable `elemVarSet` free
                ]
        bodyType matches =
            foldr
                (\arg -> mkVisFunTy (scaledMult arg) (scaledThing arg))
                (mg_res_ty matches)
                (mg_arg_tys matches)

data AllocationScope = AllocationScope
    { ownerVariables :: [TyVar]
    , checkedGivens :: [Type]
    , nameableDeclaredGivens :: Maybe [Type]
    }

emptyAllocationScope :: AllocationScope
emptyAllocationScope = AllocationScope [] [] (Just [])

extendAllocationScope ::
    Bool -> AllocationScope -> AllocationScope -> AllocationScope
extendAllocationScope signed owner outer = AllocationScope vars givens named
    where
        vars = ownerVariables outer ++ ownerVariables owner
        givens = checkedGivens outer ++ checkedGivens owner
        declared = nameableDeclaredGivens
        named
            | null (ownerVariables owner) = declared outer
            | signed
            , Set.size (Set.fromList (map getOccString vars)) == length vars =
                (++) <$> declared outer <*> declared owner
            | otherwise = Nothing

functionGivens :: (Type -> Type) -> FunctionType -> AllocationScope
functionGivens adjust info = case tcMatchTy body (functionBodyType info) of
    Just substitution ->
        let instantiate = substTy substitution
            declared = map (adjust . instantiate) predicates
            actual = map (getTyVar_maybe . instantiate . mkTyVarTy) formal
            named =
                actual == map Just owned
                    && map getOccString owned == map getOccString formal
            checked = map adjust (checkedPredicates (functionEvidence info))
         in AllocationScope
                owned
                (declared ++ checked)
                (if named then Just declared else Nothing)
    Nothing -> AllocationScope owned [] Nothing
    where
        owned = functionVariables info
        (formal, predicates, body) = tcSplitSigmaTy (functionDeclaredType info)

wrapperEvidence :: HsWrapper -> ([Id], [EvBind])
wrapperEvidence (WpCompose left right) =
    wrapperEvidence left <> wrapperEvidence right
wrapperEvidence (WpEvLam given) = ([given], [])
wrapperEvidence (WpLet (EvBinds bindings)) = ([], bagToList bindings)
wrapperEvidence _ = ([], [])

checkedPredicates :: ([Id], [EvBind]) -> [Type]
checkedPredicates (variables, bindings) =
    map idType roots ++ map (idType . eb_lhs) (close initial bindings)
    where
        roots = [v | v <- variables, isId v, isPredTy (idType v)]
        initial = Set.fromList roots
        close known remaining =
            let (ready, pending) = partition (proved known) remaining
                next = known <> Set.fromList (map eb_lhs ready)
             in ready ++ if null ready then [] else close next pending
        proved known (EvBind identifier (EvExpr rhs) _) =
            isId identifier
                && isPredTy (idType identifier)
                && all (available known) (nonDetEltsUniqSet (exprFreeIds rhs))
        proved _ _ = False
        available known identifier =
            identifier `Set.member` known
                || ( isGlobalId identifier
                        && ( isDFunId identifier
                                || isJust (isClassOpId_maybe identifier)
                           )
                   )

scopedVariables :: TypecheckedModule -> Map.Map Name [TyVar]
scopedVariables = Map.map functionVariables . functionTypes

lexicalAbstractions ::
    (Data a) => ([Id], [EvBind]) -> a -> [(([Id], [EvBind]), AbsBinds)]
lexicalAbstractions inherited value
    | Just abstraction <- cast value =
        let evidence =
                inherited
                    <> ( abs_ev_vars abstraction
                       , concatMap immutableBindings (abs_ev_binds abstraction)
                       )
         in (evidence, abstraction)
                : lexicalAbstractions evidence (abs_binds abstraction)
    | Just (FunBind (wrapper, _) _ matches) <-
        cast value :: Maybe (HsBind GhcTc) =
        lexicalAbstractions (inherited <> wrapperEvidence wrapper) matches
    | isJust (cast value :: Maybe Var)
        || isJust (cast value :: Maybe Type)
        || isJust (cast value :: Maybe HsWrapper) =
        []
    | otherwise = concat (gmapQ (lexicalAbstractions inherited) value)
    where
        immutableBindings (EvBinds bindings) = bagToList bindings
        immutableBindings _ = []

wrapperVariables :: HsWrapper -> [TyVar]
wrapperVariables (WpCompose left right) =
    wrapperVariables left ++ wrapperVariables right
wrapperVariables (WpTyLam variable) = [variable]
wrapperVariables _ = []

hasFreeVariables :: Type -> Bool
hasFreeVariables = not . isEmptyVarSet . tyCoVarsOfType

hasFreshAnnotationVariables :: LHsSigWcType GhcRn -> Bool
hasFreshAnnotationVariables annotation =
    not (null (hsOuterTyVarNames (sig_bndrs (unLoc (hswc_body annotation)))))
