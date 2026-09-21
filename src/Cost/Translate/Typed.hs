-- | Inferred type evidence for the renamed source tree.
--
-- 'expressionTypes' obtains GHC's exact types at variable uses and
-- explicit type annotations by desugaring only those selected locations
-- and reading 'GHC.Core.Utils.exprType' off the results; when wrapper
-- expressions share a source span, the outer instantiated expression is
-- kept (see the inline note). Only the resulting type map is retained —
-- the desugared Core is never used for instrumentation.
--
-- 'hasFreeVariables' flags types whose variables are not pinned by the
-- surrounding context: such evidence is attached only where the source
-- carried its own annotation, and 'hasFreshAnnotationVariables' rejects
-- annotations that introduce fresh variables of their own.
module Cost.Translate.Typed (expressionTypes, hasFreeVariables, hasFreshAnnotationVariables) where

import GHC hiding (exprType)
import GHC.Core.Utils (exprType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Types.Var.Set (isEmptyVarSet)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Ask GHC for exact types at variable uses and explicit annotations instead of
-- reconstructing its wrapper/subsumption rules. Only type evidence is retained;
-- instrumentation continues to use the renamed source tree.
expressionTypes :: HscEnv -> TypecheckedModule -> [LHsExpr GhcRn] -> [LHsExpr GhcTc] -> IO (Map.Map String Type)
expressionTypes session checked source typed = do
    let locations = Set.fromList [location e | e <- source, interesting (unLoc e)]
        -- Keep the outer, instantiated expression if wrappers share a span.
        selected = Map.fromListWith (\_ outer -> outer)
            [(location e,e) | e <- typed, location e `Set.member` locations]
    (_, result) <- initDs session (fst (tm_internals_ checked)) $
        traverse (fmap exprType . dsLExpr) selected
    maybe (fail "GHC type-evidence desugaring failed") pure result
  where
    location = showSDocUnsafe . ppr . getLocA
    interesting HsVar {} = True
    interesting ExprWithTySig {} = True
    interesting _ = False

hasFreeVariables :: Type -> Bool
hasFreeVariables = not . isEmptyVarSet . tyCoVarsOfType

hasFreshAnnotationVariables :: LHsSigWcType GhcRn -> Bool
hasFreshAnnotationVariables = not . null . hsOuterTyVarNames . sig_bndrs . unLoc . hswc_body
