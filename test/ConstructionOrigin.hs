-- | Origin status removes ordinary body obligations, never term-use obligations.
-- Inspect expressions before support certificates or Core can erase them.
module ConstructionOrigin (originReferences) where

import ConstructionAst (checkedModule, collectAst, pretty)
import ConstructionRuntime (describeName)
import GHC
import GHC.Types.Name (isDataConName, nameModule_maybe)

originReferences :: [Module] -> [TypecheckedModule] -> [String]
originReferences origins modules =
    [pretty at ++ ": ordinary originating-source term reference " ++ describeName name ++
        " is forbidden in generated code"
    | checked <- modules, checkedModule checked `notElem` origins
    , Just (group, _, _, _, _) <- [tm_renamed_source checked]
    , expression <- collectAst group :: [LHsExpr GhcRn]
    , (at, name) <- case unLoc expression of
        HsVar _ n -> [(getLocA expression, unLoc n)]
        HsRecSel _ field -> [(getLocA expression, foExt field)]
        _ -> []
    , not (isDataConName name), Just owner <- [nameModule_maybe name]
    , owner `elem` origins]
