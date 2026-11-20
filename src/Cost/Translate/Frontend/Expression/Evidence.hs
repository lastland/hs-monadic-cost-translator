module Cost.Translate.Frontend.Expression.Evidence
    ( ExpressionEvidence (..)
    , checkedExpressions
    ) where

import Control.Monad (guard)
import Cost.Translate.Frontend.Syntax (collect)
import Cost.Translate.Primitive (Primitive (..), primitive)
import Cost.Translate.Sharing.Specialization (closedScalar)
import Cost.Translate.Typed (SourceKey, sourceKey)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC hiding (exprType)
import GHC.Builtin.Names (fromIntegerName, fromRationalName)
import GHC.Builtin.Types (doubleDataCon, floatDataCon)
import GHC.Core qualified as Core
import GHC.Core.ConLike (ConLike (RealDataCon))
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types (tcg_ev_binds)
import GHC.Tc.Types.Evidence
    ( EvBind (..)
    , EvBindInfo (..)
    , EvTerm (..)
    , HsWrapper (..)
    , TcEvBinds (..)
    )
import GHC.Types.Id (isDFunId)
import GHC.Types.Name (nameModule_maybe)
import GHC.Unit.Types (bignumUnit, ghcInternalUnit, primUnit)

type Aliases = Map.Map Id Id

data ExpressionEvidence = ExpressionEvidence
    { occurrenceTypes :: Map.Map SourceKey Type
    , certifiedScalars :: Map.Map SourceKey Type
    }

checkedExpressions ::
    TypecheckedModule ->
    [LHsExpr GhcRn] ->
    [LHsExpr GhcTc] ->
    Map.Map SourceKey Core.CoreExpr ->
    ExpressionEvidence
checkedExpressions checked source typed cores =
    ExpressionEvidence
        (Map.map exprType cores)
        (checkedScalarTypes checked source typed cores)

checkedScalarTypes ::
    TypecheckedModule ->
    [LHsExpr GhcRn] ->
    [LHsExpr GhcTc] ->
    Map.Map SourceKey Core.CoreExpr ->
    Map.Map SourceKey Type
checkedScalarTypes checked source typed cores =
    Map.mapMaybe (operatorType aliases) cores
        `Map.union` Map.mapMaybeWithKey number (literals typed)
    where
        aliases = dictionaryAliases checked
        original = literals source
        number site [literal] = do
            [renamed] <- Map.lookup site original
            guard (ol_val renamed == ol_val literal)
            literalType aliases literal
        number _ _ = Nothing
        literals ::
            [LHsExpr (GhcPass p)] -> Map.Map SourceKey [HsOverLit (GhcPass p)]
        literals expressions =
            Map.fromListWith
                (++)
                [ (sourceKey site, [literal])
                | e@(L _ (HsOverLit _ literal)) <- expressions
                , let site = getLocA e
                , isGoodSrcSpan site
                ]

operatorType :: Aliases -> Core.CoreExpr -> Maybe Type
operatorType aliases core = do
    (Core.Var method, arguments) <- pure (Core.collectArgs core)
    operation <- primitive (getName method)
    case (operation, arguments) of
        (StrictOperator _, [Core.Type ty, Core.Var dictionary]) ->
            guard (knownDictionary aliases method ty dictionary)
        (Conjunction, []) -> pure ()
        (Disjunction, []) -> pure ()
        _ -> Nothing
    pure (exprType core)

literalType :: Aliases -> HsOverLit GhcTc -> Maybe Type
literalType aliases literal = do
    OverLitTc False witness ty <- pure (ol_ext literal)
    methodName <- case ol_val literal of
        HsIntegral {} -> Just fromIntegerName
        HsFractional {} -> Just fromRationalName
        _ -> Nothing
    guard (closedScalar ty)
    case witness of
        HsLit _ HsInt {} -> pure ty
        HsLit _ HsInteger {} -> pure ty
        HsApp _ (L _ (XExpr (ConLikeTc (RealDataCon con) _ _))) argument
            | con == floatDataCon
            , HsLit _ HsFloatPrim {} <- unLoc argument ->
                pure ty
            | con == doubleDataCon
            , HsLit _ HsDoublePrim {} <- unLoc argument ->
                pure ty
        HsApp _ (L _ (XExpr (WrapExpr (HsWrap wrapper (HsVar _ n))))) _
            | WpCompose (WpEvApp (EvExpr (Core.Var d))) (WpTyApp actual) <-
                wrapper
            , getName (unLoc n) == methodName
            , eqType ty actual
            , knownDictionary aliases (unLoc n) ty d ->
                pure ty
        _ -> Nothing

dictionaryAliases :: TypecheckedModule -> Aliases
dictionaryAliases checked =
    Map.fromList
        [ (lhs, rhs)
        | EvBind lhs (EvExpr (Core.Var rhs)) (EvBindWanted True) <-
            global ++ nested
        ]
    where
        global = bagToList (tcg_ev_binds (fst (tm_internals_ checked)))
        local = collect (tm_typechecked_source checked)
        nested = concat [bagToList bs | EvBinds bs <- local]

knownDictionary :: Aliases -> Id -> Type -> Id -> Bool
knownDictionary aliases method ty dictionary = case predicate of
    Just (cls, [actual]) ->
        Just cls == isClassOpId_maybe method
            && closedScalar ty
            && eqType ty actual
            && follow Set.empty dictionary
    _ -> False
    where
        predicate = getClassPredTys_maybe (idType dictionary)
        follow seen proof
            | isGlobalId proof
            , isDFunId proof =
                eqType (idType proof) (idType dictionary)
                    && maybe False standard (nameModule_maybe (getName proof))
            | proof `Set.notMember` seen
            , Just next <- Map.lookup proof aliases =
                follow (Set.insert proof seen) next
            | otherwise = False
        standard = (`elem` [primUnit, ghcInternalUnit, bignumUnit]) . moduleUnit
