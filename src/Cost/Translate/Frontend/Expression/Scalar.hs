module Cost.Translate.Frontend.Expression.Scalar
    ( ScalarCall (..)
    , scalarCall
    , scalarLiteral
    ) where

import Control.Monad (guard)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Primitive, locatedPrimitive)
import Cost.Translate.Sharing.Specialization (closedScalar)
import Cost.Translate.Typed (hasFreshAnnotationVariables, sourceKey)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import GHC
import GHC.Builtin.Types
    ( boolTy
    , charTy
    , falseDataCon
    , trueDataCon
    , unitDataCon
    , unitTy
    )
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitFunTys)

data ScalarCall = ScalarCall
    { scalarSite :: LHsExpr GhcRn
    , scalarOperation :: Primitive
    , scalarOperandType :: Type
    , scalarResultType :: Type
    , scalarLeft :: LHsExpr GhcRn
    , scalarRight :: LHsExpr GhcRn
    }

scalarCall :: Env -> LHsExpr GhcRn -> Maybe ScalarCall
scalarCall env located = case unLoc located of
    HsPar _ inner -> scalarCall env inner
    ExprWithTySig _ inner signature -> do
        call <- scalarCall env inner
        checkSignature env located signature (scalarResultType call)
        pure call
    _ -> do
        (operator, operation, left, right) <- locatedPrimitive located
        guard (isNothing (lookupEnv (unLoc operator) env))
        ty <- Map.lookup (sourceKey (getLoc operator)) (scalarEvidence env)
        let (arguments, result) = splitFunTys ty
        [a, b] <- pure (map scaledThing arguments)
        guard (closedScalar a && eqType a b && closedScalar result)
        pure (ScalarCall located operation a result left right)

scalarLiteral :: Env -> Type -> LHsExpr GhcRn -> Maybe O.Expr
scalarLiteral env expected located = case unLoc located of
    HsPar _ inner -> scalarLiteral env expected inner
    ExprWithTySig _ inner signature -> do
        checkSignature env located signature expected
        scalarLiteral env expected inner
    HsOverLit _ literal -> do
        actual <- Map.lookup (sourceKey (getLocA located)) (scalarEvidence env)
        guard (eqType expected actual)
        rendered <- either (const Nothing) Just (renderType [] actual)
        pure (O.ann (O.overLit (ol_val literal)) rendered)
    HsLit _ (HsChar _ c) | eqType expected charTy -> Just (O.char c)
    HsVar _ name
        | unLoc name == getName trueDataCon
        , eqType expected boolTy ->
            Just (O.v "True")
        | unLoc name == getName falseDataCon
        , eqType expected boolTy ->
            Just (O.v "False")
        | unLoc name == getName unitDataCon
        , eqType expected unitTy ->
            Just (O.tuple [])
    _ -> Nothing

checkSignature ::
    Env -> LHsExpr GhcRn -> LHsSigWcType GhcRn -> Type -> Maybe ()
checkSignature env located signature expected = do
    guard (not (hasFreshAnnotationVariables signature))
    (actual, _) <-
        Map.lookup (sourceKey (getLocA located)) (inferredExpressions env)
    guard (closedScalar actual && eqType actual expected)
