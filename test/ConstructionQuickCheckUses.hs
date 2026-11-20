-- | A parameter has no declaration role. Its ordinary uses still have a
-- checked type, including the instantiation selected at each occurrence.
module ConstructionQuickCheckUses
    (quickCheckPackage, quickCheckResult, QuickCheckUses, quickCheckUses, valueAt, strictPatternAt) where

import ConstructionAst (appliedDataCon, collectAst, typedVariable)
import ConstructionRuntime (resolveType)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConImplBangs, isBanged)
import GHC.Core.TyCon (isDataTyCon)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (mkFastString)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (hsLPatType, lhsExprType)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.Id.Make (seqId)
import GHC.Types.Name (isInternalName)
import GHC.Types.SrcLoc (getBufSpan)

quickCheckPackage :: String
quickCheckPackage = "QuickCheck"

quickCheckResult :: Ghc (Type -> Bool)
quickCheckResult = do
    qc <- findModule (mkModuleName "Test.QuickCheck") (Just (mkFastString quickCheckPackage))
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
quickCheckUses :: (Type -> Bool) -> TypecheckedModule -> QuickCheckUses
quickCheckUses result = inModule
  where
    inModule checked = QuickCheckUses used forced
      where
        source = tm_typechecked_source checked
        expressions = collectAst source
        sites = Set.fromList (mapMaybe site expressions)
        -- Passing a local value unchanged into a lazy field does not force it.
        -- The field's expression and every later consumer are still checked.
        forwarded = Set.fromList [key | L _ e <- expressions, x <- lazyFields e,
            Just key@(_, n) <- [variableSite x], isInternalName n, null (fst (splitPiTys (lhsExprType x)))]
        bangs = Set.fromList [span' | pat@(L _ BangPat{}) <- collectAst source :: [LPat GhcTc],
            result (hsLPatType pat), Just span' <- [inputSpan (getLocA pat)]]
        used location name = maybe False (\span' -> Set.member (span', name) sites) (inputSpan location)
        forced location = maybe False (`Set.member` bangs) (inputSpan location)
        -- For `seq x ...` report the QC operand itself. In a section or
        -- bare specialization there is no source operand to report.
        applied = Set.fromList [key | HsApp _ f _ <- collectAst source :: [HsExpr GhcTc],
            Just key <- [variableSite f]]
        site expression = do
            key@(_, name) <- variableSite expression
            let ty = lhsExprType expression
                bareSeq = name == getName seqId && Set.notMember key applied && case fst (splitFunTys (snd (splitForAllTyVars ty))) of
                    arg:_ -> let operand = scaledThing arg in null (fst (splitPiTys operand)) && result operand
                    [] -> False
            if (result ty && Set.notMember key forwarded) || bareSeq then Just key else Nothing
    variableSite expression = do
        (location, name) <- typedVariable expression
        (,name) <$> inputSpan location
    -- The two compiler passes read the same buffer. LINE pragmas can give
    -- distinct operations the same diagnostic position, even in one module.
    inputSpan location = case getBufSpan location of
        Strict.Just bytes -> Just bytes
        Strict.Nothing -> Nothing

lazyFields :: HsExpr GhcTc -> [LHsExpr GhcTc]
lazyFields (ExplicitList _ xs) = xs
lazyFields (ExplicitTuple _ fields Boxed) = [x | Present _ x <- fields]
lazyFields RecordCon{rcon_con = L _ (RealDataCon con), rcon_flds = fields}
    | isDataTyCon (dataConTyCon con) =
        [hfbRHS field | L _ field <- rec_flds fields,
            let name = getName (foExt (unLoc (hfbLHS field))),
            Just bang <- [lookup name (zip (map flSelector (dataConFieldLabels con)) (dataConImplBangs con))],
            not (isBanged bang)]
lazyFields e | Just (con, args) <- appliedDataCon e, isDataTyCon (dataConTyCon con) =
    [x | (bang, x) <- zip (dataConImplBangs con) args, not (isBanged bang)]
lazyFields _ = []
