-- | A parameter has no declaration role. Its ordinary uses still have a
-- checked type, including the instantiation selected at each occurrence.
module ConstructionQuickCheckUses (quickCheckPackage, quickCheckResult, quickCheckUses) where

import ConstructionAst (collectAst, typedVariable)
import ConstructionRuntime (resolveType)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.Type (splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (fsLit)
import GHC.Hs.Syn.Type (lhsExprType)

quickCheckPackage :: String
quickCheckPackage = "QuickCheck-2.17.1.0"

quickCheckResult :: Ghc (Type -> Bool)
quickCheckResult = do
    qc <- findModule (mkModuleName "Test.QuickCheck") (Just (fsLit "QuickCheck"))
    constructors <- mapM (resolveType qc) ["Gen", "Property"]
    pure $ \ty -> case splitTyConApp_maybe (snd (splitPiTys ty)) of
        Just (constructor, _) -> constructor `elem` constructors
        Nothing -> False

-- Index expressions, not binder Ids or declared argument types. An unused
-- argument is inert. The renamed walk queries only sites in ordinary bodies.
quickCheckUses :: (Type -> Bool) -> TypecheckedModule -> SrcSpan -> Name -> Bool
quickCheckUses result checked = used
  where
    sites = Set.fromList (mapMaybe site (collectAst (tm_typechecked_source checked)))
    site expression = case typedVariable expression of
        Just (RealSrcSpan location _, name) | result (lhsExprType expression) -> Just (location, name)
        _ -> Nothing
    used (RealSrcSpan location _) name = Set.member (location, name) sites
    used _ _ = False
