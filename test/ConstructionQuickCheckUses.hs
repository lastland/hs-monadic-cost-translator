-- | A parameter has no declaration role. Its ordinary uses still have a
-- checked type, including the instantiation selected at each occurrence.
module ConstructionQuickCheckUses
    (quickCheckPackage, quickCheckResult, QuickCheckUses, quickCheckUses, valueAt, strictPatternAt) where

import ConstructionAst (allBindings, collectAst, typedVariable)
import ConstructionRuntime (resolveType)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as Core
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (getTyVar_maybe, isPredTy, splitForAllTyVars, splitFunTys, splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (fsLit)
import GHC.Hs.Syn.Type (hsLPatType, lhsExprType)
import GHC.Types.Id.Make (seqId)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import GHC.Types.Var (isId)
import GHC.Types.Var.Set (elemVarSet)

quickCheckPackage :: String
quickCheckPackage = "QuickCheck-2.17.1.0"

quickCheckResult :: Ghc (Type -> Bool)
quickCheckResult = do
    qc <- findModule (mkModuleName "Test.QuickCheck") (Just (fsLit "QuickCheck"))
    constructors <- mapM (resolveType qc) ["Gen", "Property"]
    pure $ \ty -> case splitTyConApp_maybe (snd (splitPiTys ty)) of
        Just (constructor, _) -> constructor `elem` constructors
        Nothing -> False

data QuickCheckUses = QuickCheckUses
    { valueAt :: SrcSpan -> Name -> Bool
    , strictPatternAt :: SrcSpan -> Bool
    }

-- The renamed walk queries these typed operations only in ordinary bodies.
-- A plain binder is inert; an explicit bang is a force even on a wildcard.
quickCheckUses :: (Type -> Bool) -> [Core.CoreProgram] -> TypecheckedModule -> QuickCheckUses
quickCheckUses result programs = inModule
  where
    slots = usedGenericArguments programs
    inModule checked = QuickCheckUses used forced
      where
        source = tm_typechecked_source checked
        sites = Set.fromList (mapMaybe site (collectAst source))
        bangs = Set.fromList [span' | pat@(L _ BangPat{}) <- collectAst source :: [LPat GhcTc],
            result (hsLPatType pat), RealSrcSpan span' _ <- [getLocA pat]]
        used location name = maybe False (\span' -> Set.member (span', name) sites) (srcSpanToRealSrcSpan location)
        forced location = maybe False (`Set.member` bangs) (srcSpanToRealSrcSpan location)
        -- For `seq x ...` report the QC operand itself. In a section or
        -- bare specialization there is no source operand to report.
        applied = Set.fromList [key | HsApp _ f _ <- collectAst source :: [HsExpr GhcTc],
            Just key <- [variableSite f]]
        site expression = do
            key@(_, name) <- variableSite expression
            let ty = lhsExprType expression
                positions | name == getName seqId = [0 | Set.notMember key applied]
                          | otherwise = Map.findWithDefault [] name slots
                args = map scaledThing (fst (splitFunTys (snd (splitForAllTyVars ty))))
                specialized = or [i `elem` positions && firstOrder arg && result arg
                    | (i, arg) <- zip [0..] (filter (not . isPredTy) args)]
            if result ty || (not (null positions) && specialized) then Just key else Nothing
    variableSite expression = do
        (location, name) <- typedVariable expression
        (,name) <$> srcSpanToRealSrcSpan location

firstOrder :: Type -> Bool
firstOrder = null . fst . splitPiTys

-- At `ordinary = strictAny @(Gen Bool)` there is no source operand either.
-- Only summarize direct first-order home lambdas: which bare type-variable
-- arguments actually occur in the body? No call graph or callback is followed.
usedGenericArguments :: [Core.CoreProgram] -> Map.Map Name [Int]
usedGenericArguments programs = Map.fromList
    [(getName identifier, positions) | (identifier, rhs) <- concatMap allBindings programs,
        let (binders, body) = Core.collectBinders rhs
            terms = [v | v <- binders, isId v, not (isPredTy (idType v))]
            free = exprFreeVars body
            positions = [i | (i, v) <- zip [0..] terms, Just tv <- [getTyVar_maybe (idType v)],
                tv `elem` binders, v `elemVarSet` free],
        all (firstOrder . idType) terms, not (null positions)]
