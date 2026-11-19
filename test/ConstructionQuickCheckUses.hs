-- | A parameter has no declaration role. Its ordinary uses still have a
-- checked type, including the instantiation selected at each occurrence.
module ConstructionQuickCheckUses
    (quickCheckPackage, quickCheckResult, QuickCheckUses, quickCheckUses, valueAt, strictPatternAt) where

import ConstructionAst (collectAst, typedVariable)
import ConstructionRuntime (resolveType)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (mkFastString)
import GHC.Hs.Syn.Type (hsLPatType, lhsExprType)
import GHC.Types.Id.Make (seqId)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)

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
                bareSeq = name == getName seqId && Set.notMember key applied && case fst (splitFunTys (snd (splitForAllTyVars ty))) of
                    arg:_ -> let operand = scaledThing arg in null (fst (splitPiTys operand)) && result operand
                    [] -> False
            if result ty || bareSeq then Just key else Nothing
    variableSite expression = do
        (location, name) <- typedVariable expression
        (,name) <$> srcSpanToRealSrcSpan location
