-- | Associate actual stock requests with unique actual instance DFuns.
-- This is module metadata, not evidence or a permission for a method body.
module ConstructionStockRequests (stockProvenance) where

import ConstructionAst (collectAst, pretty)
import ConstructionRuntime (Runtime, standardNames)
import Control.Exception (try)
import Control.Monad (forM, guard)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import qualified Data.Set as Set
import GHC hiding (typeKind)
import GHC.Core.Class (classAllSelIds)
import GHC.Core.InstEnv (is_cls, is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isDataTyCon, isFamInstTyCon, tyConResKind)
import GHC.Core.Type (getTyVar_maybe, isLiftedTypeKind, splitForAllTyVars,
    splitFunTys, splitTyConApp_maybe, substTyWith, typeKind)
import GHC.Driver.Env (hscSetFlags)
import GHC.Types.SourceError (SourceError)
import GHC.Tc.Gen.HsType (tcHsClsInstType)
import GHC.Tc.Types (tcg_insts, tcg_tcs, tcg_top_loc)
import GHC.Tc.Types.Origin (UserTypeCtxt(..))
import GHC.Tc.Utils.Monad (initTcWithGbl, setSrcSpan)
import GHC.Tc.Zonk.Type (zonkTcTypeToType)
import GHC.Types.Var (varType)

stockProvenance :: Runtime -> TypecheckedModule -> Ghc (Set.Set Name)
stockProvenance rt checked = case tm_renamed_source checked of
    Nothing -> pure Set.empty
    Just (group, _, _, _, _) -> do
        session <- getSession
        let global = fst (tm_internals_ checked)
            configured = hscSetFlags (ms_hspp_opts (pm_mod_summary (tm_parsed_module checked))) session
            instances = tcg_insts global
            dfuns = map is_dfun instances
            primitive cls = any (`elem` map getName (classAllSelIds cls))
                [n | label <- ["==", "showsPrec"], Just n <- [lookup ("Prelude." ++ label) (standardNames rt)]]
            classes = nub [getName (is_cls inst) | inst <- instances, primitive (is_cls inst)]
            standalone = [request | request <- collectAst group :: [LDerivDecl GhcRn],
                stock (deriv_strategy (unLoc request)),
                signature <- collectAst (deriv_type (unLoc request)) :: [LHsSigType GhcRn],
                Just name <- [className (sig_body (unLoc signature))], name `elem` classes]
            attached = [(name, tc) |
                DataDecl{tcdLName = target, tcdDataDefn = definition} <-
                    collectAst group :: [TyClDecl GhcRn],
                tc <- tcg_tcs global, getName tc == unLoc target, ordinary tc,
                L _ clause <- dd_derivs definition, stock (deriv_clause_strategy clause),
                signature <- collectAst (deriv_clause_tys clause) :: [LHsSigType GhcRn],
                Just name <- [className (sig_body (unLoc signature))], name `elem` classes]
        standaloneNames <- fmap concat $ forM standalone $ \request -> do
            let signatures = collectAst (deriv_type (unLoc request)) :: [LHsSigType GhcRn]
                unavailable :: Ghc a
                unavailable = fail (pretty (getLocA request) ++ ": stock request metadata unavailable")
            signature <- case signatures of
                [one] -> pure one
                _ -> unavailable
            result <- liftIO $ try @SourceError $ initTcWithGbl configured global (tcg_top_loc global) $
                setSrcSpan (getLocA request) $
                    tcHsClsInstType (InstDeclCtxt True) signature >>= zonkTcTypeToType
            ty <- case result of
                Right (_, Just actual) -> pure actual
                _ -> unavailable
            case genericHead ty of
                Nothing -> pure []
                Just _ -> case [df | df <- dfuns, sameHead ty (idType df)] of
                    [df] | sameContext ty (idType df) -> pure [getName df]
                    [] -> fail (pretty (getLocA request) ++ ": no unique actual stock DFun")
                    [_] -> fail (pretty (getLocA request) ++ ": stock request context mismatch")
                    _ -> fail (pretty (getLocA request) ++ ": ambiguous actual stock DFun")
        -- Attached requests have no user-written instance context to re-elaborate:
        -- the checked TyCon supplies the complete declared kind telescope and
        -- GHC's unique actual DFun supplies its inferred ordered context.
        attachedNames <- fmap concat $ forM attached $ \(name, tc) ->
            case [df | df <- dfuns, Just (cls, actual, _, _, _) <- [genericHead (idType df)],
                    getName cls == name, actual == tc] of
                [df] -> pure [getName df]
                _ -> fail (pretty (getName tc) ++ ": no unique attached stock DFun")
        pure (Set.fromList (standaloneNames ++ attachedNames))

stock :: Maybe (LDerivStrategy GhcRn) -> Bool
stock Nothing = True
stock (Just (L _ StockStrategy{})) = True
stock _ = False

-- Used only to avoid re-elaborating unrelated classes. It grants no provenance.
className :: LHsType GhcRn -> Maybe Name
className (L _ ty) = case ty of
    HsTyVar _ _ name -> Just (unLoc name)
    HsForAllTy _ _ body -> className body
    HsQualTy _ _ body -> className body
    HsParTy _ body -> className body
    HsAppTy _ fun _ -> className fun
    HsAppKindTy _ fun _ -> className fun
    _ -> Nothing

ordinary :: TyCon -> Bool
ordinary tc = isDataTyCon tc && not (isNewTyCon tc) && not (isFamInstTyCon tc) &&
    isLiftedTypeKind (tyConResKind tc) && all isVanillaDataCon (tyConDataCons tc)

genericHead :: Type -> Maybe (Class, TyCon, [TyVar], Type, [Type])
genericHead ty = do
    let (variables, body) = splitForAllTyVars ty
        (_, predicate) = splitFunTys body
    (cls, [operand]) <- getClassPredTys_maybe predicate
    (tc, arguments) <- splitTyConApp_maybe operand
    headVariables <- mapM getTyVar_maybe arguments
    guard (ordinary tc && length arguments == length (tyConTyVars tc) &&
        length headVariables == length variables &&
        length (nub headVariables) == length variables && all (`elem` variables) headVariables &&
        sameTypes (map (substTyWith (tyConTyVars tc) arguments . varType) (tyConTyVars tc))
            (map typeKind arguments))
    pure (cls, tc, headVariables, predicate, arguments)

-- Map variables by datatype-head positions, never by DFun forall order.
sameHead :: Type -> Type -> Bool
sameHead request actual = case (genericHead request, genericHead actual) of
    (Just (sourceClass, sourceTyCon, sourceVars, sourcePredicate, _),
     Just (actualClass, actualTyCon, _, actualPredicate, actualArgs)) ->
        getName sourceClass == getName actualClass && sourceTyCon == actualTyCon &&
        let apply = substTyWith sourceVars actualArgs
        in eqType (apply sourcePredicate) actualPredicate &&
            sameTypes (map (apply . varType) sourceVars) (map typeKind actualArgs)
    _ -> False

sameContext :: Type -> Type -> Bool
sameContext request actual = case (genericHead request, genericHead actual) of
    (Just (_, _, variables, _, _), Just (_, _, _, _, arguments)) ->
        let (source, _) = splitFunTys (snd (splitForAllTyVars request))
            (target, _) = splitFunTys (snd (splitForAllTyVars actual))
        in sameTypes (map (substTyWith variables arguments . scaledThing) source) (map scaledThing target)
    _ -> False

sameTypes :: [Type] -> [Type] -> Bool
sameTypes xs ys = length xs == length ys && and (zipWith eqType xs ys)
