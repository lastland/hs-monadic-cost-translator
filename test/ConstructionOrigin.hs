-- | Origin status removes ordinary body obligations, never term-use obligations.
-- Inspect expressions before support certificates or Core can erase them.
module ConstructionOrigin (originReferences) where

import ConstructionAst (checkedModule, collectAst, pretty)
import ConstructionRuntime (describeName)
import Data.List (nubBy)
import GHC
import GHC.Data.Strict qualified as Strict
import GHC.Types.Name (isDataConName, nameModule_maybe)
import GHC.Types.SrcLoc (getBufSpan)

originReferences :: [Module] -> [TypecheckedModule] -> [String]
originReferences origins modules =
    [pretty at ++ ": ordinary originating-source term reference " ++ describeName name ++
        " is forbidden in generated code"
    | checked <- modules, checkedModule checked `notElem` origins
    , Just (group, _, _, _, _) <- [tm_renamed_source checked]
    , (at, name) <- nubBy samePhysicalUse
        [(at, name)
        | expression <- collectAst group :: [LHsExpr GhcRn]
        , (at, name) <- case unLoc expression of
            HsVar _ n -> [(getLocA expression, unLoc n)]
            HsRecSel _ field -> [(getLocA expression, foExt field)]
            _ -> []
        , not (isDataConName name), Just owner <- [nameModule_maybe name]
        , owner `elem` origins]]
  where
    -- Each list belongs to one physical checked module. Logical LINE spans
    -- cannot identify a written use; unknown physical identities stay separate.
    samePhysicalUse (at, name) (other, actual)
        | name == actual, Strict.Just a <- getBufSpan at,
            Strict.Just b <- getBufSpan other = a == b
        | otherwise = False
