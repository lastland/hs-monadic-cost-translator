-- Inspect the translated allocation itself: unrelated support declarations in
-- the complete module can neither satisfy nor mask this check.
module ConstructionAllocation (checkPhantomAllocation) where

import ConstructionAst
import ConstructionRuntime (resolveExport, resolveType)
import GHC

checkPhantomAllocation :: TypecheckedModule -> Ghc ()
checkPhantomAllocation checked = do
    let owner = ms_mod (pm_mod_summary (tm_parsed_module checked))
    views <- resolveExport owner "viewsM"
    producer <- resolveExport owner "makeM"
    parcel <- resolveType owner "ParcelT"
    allocate <- findModule (mkModuleName "Memorist.Memorist") Nothing >>= (`resolveExport` "lazy")
    Just (group, _, _, _, _) <- pure (tm_renamed_source checked)
    [binding] <- pure [b | b <- collectAst group, bindingName b == Just views]
    let actions = [body | HsApp _ f body <- collectAst binding :: [HsExpr GhcRn], variable f == Just allocate]
        uses = [() | HsVar _ name <- collectAst binding :: [HsExpr GhcRn], unLoc name == producer]
        constructors = map getName (tyConDataCons parcel)
    case actions of
        [action] | length uses == 1, retagsResult producer constructors action -> pure ()
        _ -> fail "viewsM must suspend one producer invocation and return its constructor with every original field thunk"

retagsResult :: Name -> [Name] -> LHsExpr GhcRn -> Bool
retagsResult producer constructors action = case (results, cases) of
    ([result], [(scrutinee, branches)]) | variable scrutinee == Just result ->
        let alternatives = map unLoc (unLoc (mg_alts branches))
        in length alternatives == length constructors && all (forwards alternatives) constructors
    _ -> False
  where
    results = [name | L _ (BindStmt _ pat rhs) <- collectAst action :: [ExprLStmt GhcRn],
        Just _ <- [call [producer] rhs], Just name <- [plainPattern pat]]
    cases = [(value, branches) | HsCase _ value branches <- collectAst action :: [HsExpr GhcRn]]
    forwards alternatives constructor = case
        [(fields, args) | match <- alternatives, [pat] <- [m_pats match],
         Just pats <- [constructorPattern constructor pat], Just fields <- [mapM plainPattern pats],
         Just body <- [oneRhs match], Just args <- [call [constructor] body]] of
        [(fields, args)] -> mapM variable args == Just fields
        _ -> False
