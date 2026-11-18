module Cost.Translate.Typed
    ( expressionTypes
    , expressionTypeAt
    , FunctionType (..)
    , functionTypes
    , scopedVariables
    , hasFreeVariables
    , hasFreshAnnotationVariables
    ) where

import Data.Data (Data, cast, gmapQ)
import Data.List (isSuffixOf, maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Set qualified as Set
import GHC hiding (exprType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledMult, scaledThing)
import GHC.Core.Type (mkVisFunTy, splitFunTys)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types.Evidence (HsWrapper (..))
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Var (Var)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

expressionTypes ::
    HscEnv ->
    TypecheckedModule ->
    [LHsExpr GhcRn] ->
    [LHsExpr GhcTc] ->
    IO (Map.Map String Type)
expressionTypes session checked source typed = do
    let locations = Set.fromList [location e | e <- source, interesting (unLoc e)]
        original =
            Map.fromListWith
                (\_ outer -> outer)
                [(location e, e) | e <- typed, location e `Set.member` locations]
        names =
            Map.fromListWith
                (++)
                [(location e, [(unLoc name, getLocA e)]) | e@(L _ (HsVar _ name)) <- source]
        occurrences = Map.fromListWith (+) [(location e, 1 :: Int) | e <- typed]
        references =
            Map.fromListWith
                (++)
                [ (showSDocUnsafe (ppr (last path)), [(name, path, e)])
                | e <- typed
                , Just (name, path) <- [reference e]
                ]
        selected = Map.mapMaybeWithKey (recover names occurrences references) original
    (_, result) <-
        initDs session (fst (tm_internals_ checked)) $
            traverse (fmap exprType . dsLExpr) selected
    maybe (fail "GHC type-evidence desugaring failed") pure result
    where
        location = showSDocUnsafe . ppr . getLocA
        interesting HsVar {} = True
        interesting ExprWithTySig {} = True
        interesting _ = False
        recover names occurrences references site original = case Map.lookup site references of
            Just candidates | any (\(_, path, _) -> length path > 1) candidates -> do
                [(name, sourceSpan)] <- Map.lookup site names
                let (_, outerPath, outer) = maximumBy (comparing (\(_, path, _) -> length path)) candidates
                if last outerPath == sourceSpan
                    && all
                        (\(checkedName, path, _) -> checkedName == name && path `isSuffixOf` outerPath)
                        candidates
                    && length candidates == length outerPath
                    && all
                        (\span' -> Map.lookup (showSDocUnsafe (ppr span')) occurrences == Just 1)
                        outerPath
                    then Just outer
                    else Nothing
            _ -> Just original
        reference expression = case getLocA expression of
            RealSrcSpan {} -> do
                (name, path) <- nested (unLoc expression)
                pure (name, getLocA expression : path)
            _ -> Nothing
        nested (HsVar _ name)
            | let (_, _, tau) = tcSplitSigmaTy (idType (unLoc name))
            , not (null (fst (splitFunTys tau))) =
                Just (getName (unLoc name), [])
        nested (HsPar _ expression) = reference expression
        nested (XExpr (WrapExpr (HsWrap wrapper expression)))
            | applications wrapper = nested expression
        nested _ = Nothing
        applications (WpCompose left right) = applications left && applications right
        applications WpTyApp {} = True
        applications WpEvApp {} = True
        applications WpHole = True
        applications _ = False

expressionTypeAt ::
    [LHsExpr GhcRn] -> Map.Map String Type -> LHsExpr GhcRn -> Maybe Type
expressionTypeAt source hints = select
    where
        locations = Map.fromListWith (+) [(location expression, 1 :: Int) | expression <- source]
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
                | variable <- abs_tvs abstraction
                , variable `elemVarSet` tyCoVarsOfType (idType (abe_mono export))
                ]
        bodyType matches =
            foldr
                (\argument -> mkVisFunTy (scaledMult argument) (scaledThing argument))
                (mg_res_ty matches)
                (mg_arg_tys matches)

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
wrapperVariables (WpCompose left right) = wrapperVariables left ++ wrapperVariables right
wrapperVariables (WpTyLam variable) = [variable]
wrapperVariables _ = []

hasFreeVariables :: Type -> Bool
hasFreeVariables = not . isEmptyVarSet . tyCoVarsOfType

hasFreshAnnotationVariables :: LHsSigWcType GhcRn -> Bool
hasFreshAnnotationVariables = not . null . hsOuterTyVarNames . sig_bndrs . unLoc . hswc_body
