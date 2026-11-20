-- | A parameter has no declaration role. Its ordinary uses still have a
-- checked type, including the instantiation selected at each occurrence.
module ConstructionQuickCheckUses
    (quickCheckPackage, quickCheckResult, quickCheckOperand, QuickCheckUses, quickCheckUses, valueAt, strictPatternAt) where

import ConstructionAst (appliedDataCon, collectAst, spine, strictConstructorOperands, strictNullaryRhs, typedVariable, updateAlternatives)
import ConstructionRuntime (resolveType)
import Data.Foldable (toList)
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
import GHC.Types.Name.Reader (greName)
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

-- Forcing a function to WHNF does not evaluate its result.
quickCheckOperand :: (Type -> Bool) -> Type -> Bool
quickCheckOperand result ty = null (fst (splitPiTys ty)) && result ty

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
        forwarded = Set.fromList $ concatMap (mapMaybe variableSite . lazyFields) expressions ++
            [(bytes, unLoc n) | e <- concatMap lazyUpdateFields (collectAst source),
                (L _ (HsVar _ n), []) <- [spine e], Just bytes <- [inputSpan (getLocA n)]]
        bangs = Set.fromList $
            [span' | pat@(L _ BangPat{}) <- collectAst source :: [LPat GhcTc],
                quickCheckOperand result (hsLPatType pat), Just span' <- [inputSpan (getLocA pat)]] ++
            [span' | FunBind _ n matches <- collectAst source :: [HsBind GhcTc],
                strictNullaryRhs matches, quickCheckOperand result (mg_res_ty (mg_ext matches)),
                Just span' <- [inputSpan (getLocA n)]]
        used location name = maybe False (\span' -> Set.member (span', name) sites) (inputSpan location)
        forced location = maybe False (`Set.member` bangs) (inputSpan location)
        -- For `seq x ...` report the QC operand itself. In a section or
        -- bare specialization there is no source operand to report.
        applied = Set.fromList [key | HsApp _ f _ <- collectAst source :: [HsExpr GhcTc],
            Just key <- [variableSite f]]
        site expression | Just (bytes, name, fields) <- strictConstructorOperands expression, any (quickCheckOperand result) fields =
            Just (bytes, name)
        site expression = do
            key@(_, name) <- variableSite expression
            let ty = lhsExprType expression
                stored = isInternalName name && null (fst (splitPiTys ty)) && Set.member key forwarded
                bareSeq = name == getName seqId && Set.notMember key applied && case fst (splitFunTys (snd (splitForAllTyVars ty))) of
                    arg:_ -> quickCheckOperand result (scaledThing arg)
                    [] -> False
            if (result ty && not stored) || bareSeq then Just key else Nothing
    variableSite expression = do
        (location, name) <- typedVariable expression
        (,name) <$> inputSpan location
    -- The two compiler passes read the same buffer. LINE pragmas can give
    -- distinct operations the same diagnostic position, even in one module.
    inputSpan location = case getBufSpan location of
        Strict.Just bytes -> Just bytes
        Strict.Nothing -> Nothing

lazyFields :: LHsExpr GhcTc -> [LHsExpr GhcTc]
lazyFields (L _ (ExplicitList _ xs)) = xs
lazyFields (L _ (ExplicitTuple _ fields Boxed)) = [x | Present _ x <- fields]
lazyFields (L _ RecordCon{rcon_con = L _ (RealDataCon con), rcon_flds = fields})
    | isDataTyCon (dataConTyCon con) =
        [hfbRHS field | L _ field <- rec_flds fields,
            let name = getName (foExt (unLoc (hfbLHS field))),
            Just bang <- [lookup name (fieldBangs con)],
            not (isBanged bang)]
lazyFields e | Just (con, args) <- appliedDataCon e, isDataTyCon (dataConTyCon con) =
    [x | (bang, x) <- zip (dataConImplBangs con) args, not (isBanged bang)]
lazyFields _ = []

fieldBangs :: DataCon -> [(Name, HsImplBang)]
fieldBangs con = zip (map flSelector (dataConFieldLabels con)) (dataConImplBangs con)

-- A shared record label can be strict in one constructor and lazy in another.
-- Require the actual selector to be lazy in every alternative of this update.
lazyUpdateFields :: XXExprGhcTc -> [LHsExpr GhcRn]
lazyUpdateFields (ExpandedThingTc (OrigExpr RecordUpd{rupd_flds = RegularRecUpdFields parents fields}) expanded)
    | not (null alternatives) =
        [hfbRHS (unLoc field) | parent <- toList parents, let labels = toList (rnRecUpdLabels parent),
            length labels == length fields, (field, label) <- zip fields labels,
            all (stores (greName label) . fst) alternatives]
  where
    alternatives = updateAlternatives expanded
    stores name (RealDataCon con) = isDataTyCon (dataConTyCon con) &&
        maybe False (not . isBanged) (lookup name (fieldBangs con))
    stores _ _ = False
lazyUpdateFields _ = []
