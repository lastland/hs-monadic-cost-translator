module Cost.Translate.Typed
    ( expressionTypes
    , expressionTypeAt
    , FunctionType (..)
    , functionTypes
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
import GHC.Core.FVs (exprFreeIds)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledMult, scaledThing)
import GHC.Core.TyCo.Subst (substTy)
import GHC.Core.Type (isPredTy, mkVisFunTy, splitFunTys)
import GHC.Core.Unify (tcMatchTy)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types.Evidence
    ( EvBind (..)
    , EvTerm (..)
    , HsWrapper (..)
    , TcEvBinds (..)
    )
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Id (isDFunId)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var (Var, isId)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

expressionTypes ::
    HscEnv ->
    TypecheckedModule ->
    [LHsExpr GhcRn] ->
    [LHsExpr GhcTc] ->
    IO (Map.Map String Type)
expressionTypes session checked source typed = do
    let locations =
            Set.fromList
                [location e | e <- source, interesting (unLoc e)]
        original =
            Map.fromListWith
                (\_ outer -> outer)
                [ (location e, e)
                | e <- typed
                , location e `Set.member` locations
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
                [ (showSDocUnsafe (ppr (last path)), [(name, path, e)])
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
        location = showSDocUnsafe . ppr . getLocA
        interesting HsVar {} = True
        interesting ExprWithTySig {} = True
        interesting _ = False

recover ::
    Map.Map String [(Name, SrcSpan)] ->
    Map.Map String Int ->
    Map.Map String [(Name, [SrcSpan], LHsExpr GhcTc)] ->
    String ->
    LHsExpr GhcTc ->
    Maybe (LHsExpr GhcTc)
recover names counts refs site original = case Map.lookup site refs of
    Just candidates | any (\(_, path, _) -> length path > 1) candidates -> do
        [(name, sourceSpan)] <- Map.lookup site names
        let pathLength (_, path, _) = length path
            (_, outerPath, outer) = maximumBy (comparing pathLength) candidates
            sameReference (checkedName, path, _) =
                checkedName == name && path `isSuffixOf` outerPath
            uniqueSpan span' =
                Map.lookup (showSDocUnsafe (ppr span')) counts == Just 1
        if last outerPath == sourceSpan
            && all sameReference candidates
            && length candidates == length outerPath
            && all uniqueSpan outerPath
            then Just outer
            else Nothing
    _ -> Just original
reference :: LHsExpr GhcTc -> Maybe (Name, [SrcSpan])
reference expression = case getLocA expression of
    RealSrcSpan {} -> do
        (name, path) <- nested (unLoc expression)
        pure (name, getLocA expression : path)
    _ -> Nothing
nested :: HsExpr GhcTc -> Maybe (Name, [SrcSpan])
nested (HsVar _ name)
    | let (_, _, tau) = tcSplitSigmaTy (idType (unLoc name))
    , not (null (fst (splitFunTys tau))) =
        Just (getName (unLoc name), [])
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
    [LHsExpr GhcRn] -> Map.Map String Type -> LHsExpr GhcRn -> Maybe Type
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
        location = showSDocUnsafe . ppr . getLocA

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
                        (wrapperEvidence wrapper)
                  ]
              )
            | abstraction <- checkedAbstractions (tm_typechecked_source checked)
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

functionGivens :: FunctionType -> [Type]
functionGivens info = case tcMatchTy body (functionBodyType info) of
    Just substitution ->
        let givens = map (substTy substitution) predicates
         in givens ++ checkedPredicates givens (functionEvidence info)
    Nothing -> []
    where
        (_, predicates, body) = tcSplitSigmaTy (functionDeclaredType info)

wrapperEvidence :: HsWrapper -> ([Id], [EvBind])
wrapperEvidence (WpCompose left right) =
    wrapperEvidence left <> wrapperEvidence right
wrapperEvidence (WpEvLam given) = ([given], [])
wrapperEvidence (WpLet (EvBinds bindings)) = ([], bagToList bindings)
wrapperEvidence _ = ([], [])

checkedPredicates :: [Type] -> ([Id], [EvBind]) -> [Type]
checkedPredicates givens (variables, bindings) =
    map (idType . eb_lhs) (close initial bindings)
    where
        initial =
            Set.fromList
                [ v
                | v <- variables
                , isId v
                , any (eqType (idType v)) givens
                ]
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

checkedAbstractions :: (Data a) => a -> [AbsBinds]
checkedAbstractions value
    | Just abstraction <- cast value =
        abstraction : checkedAbstractions (abs_binds abstraction)
    | isJust (cast value :: Maybe Var)
        || isJust (cast value :: Maybe Type)
        || isJust (cast value :: Maybe HsWrapper) =
        []
    | otherwise = concat (gmapQ checkedAbstractions value)

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
