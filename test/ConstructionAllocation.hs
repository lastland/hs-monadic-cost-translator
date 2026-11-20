-- Inspect the translated allocation itself: unrelated support declarations in
-- the complete module can neither satisfy nor mask this check.
module ConstructionAllocation (checkPhantomAllocation) where

import ConstructionAst
import ConstructionRuntime (resolveExport, resolveType)
import Data.Maybe (isJust)
import GHC

checkPhantomAllocation :: TypecheckedModule -> Ghc ()
checkPhantomAllocation checked = do
    let owner = ms_mod (pm_mod_summary (tm_parsed_module checked))
    producer <- resolveExport owner "makeM"
    parcel <- resolveType owner "ParcelT"
    rt <- findModule (mkModuleName "Memorist.Memorist") Nothing
    allocate <- resolveExport rt "lazy"
    pair <- resolveExport rt "PairT"
    view <- resolveExport owner "mapThunk"
    ret <- findModule (mkModuleName "Prelude") Nothing >>= (`resolveExport` "pure")
    Just (group, _, _, _, _) <- pure (tm_renamed_source checked)
    let constructors = map getName (tyConDataCons parcel)
        inspect name = do
            ownerName <- resolveExport owner name
            [binding] <- pure [b | b <- collectAst group, bindingName b == Just ownerName]
            if singleRoot producer allocate view pair ret constructors binding then pure () else
                fail (name ++ " must suspend one producer invocation and return both instantiations of that root with every original field thunk")
    mapM_ inspect ["viewsM", "inferredM"]

singleRoot :: Name -> Name -> Name -> Name -> Name -> [Name] -> HsBind GhcRn -> Bool
singleRoot producer allocate view pair ret constructors owner = case allocators of
    [allocator] | uses producer == 1, uses allocator == 1, [root] <- roots allocator,
                  [alias] <- aliases root -> (Just alias, Just alias) `elem` pairs
    _ -> False
  where
    bindings = collectAst owner
    uses target = length [() | HsVar _ name <- collectAst owner :: [HsExpr GhcRn], unLoc name == target]
    allocators = [name | binding <- bindings, Just name <- [bindingName binding],
        Just [([parameter], body)] <- [equations binding], Just input <- [plainPattern parameter],
        Just [action] <- [call [allocate] body], retagsResult ret input constructors action]
    roots allocator = [root | L _ (BindStmt _ pat rhs) <- collectAst owner :: [ExprLStmt GhcRn],
        Just root <- [plainPattern pat], Just [action] <- [call [allocator] rhs], returnsProducer action]
    aliases root = [alias | binding <- bindings, Just alias <- [bindingName binding], Just [([], rhs)] <- [equations binding],
        Just [_, input] <- [call [view] rhs], variable input == Just root]
    pairs = [(variable a, variable b) | e <- collectAst owner :: [LHsExpr GhcRn],
        Just [value] <- [call [ret] e], Just [a,b] <- [call [pair] value]]
    returnsProducer expression = case spine expression of
        (L _ (HsDo _ (DoExpr Nothing) (L _ statements)), [])
            | L _ (LastStmt _ terminal _ _) : _ <- reverse statements -> returnsProducer terminal
        _ -> isJust (call [producer] expression)

retagsResult :: Name -> Name -> [Name] -> LHsExpr GhcRn -> Bool
retagsResult ret producer constructors action = case spine action of
    (L _ (HsDo _ (DoExpr Nothing) (L _ [L _ (BindStmt _ pat rhs), L _ (LastStmt _ end _ _)])), [])
        | variable rhs == Just producer, Just result <- plainPattern pat,
          Just [value] <- call [ret] end, (L _ (HsCase _ scrutinee branches), []) <- spine value,
          variable scrutinee == Just result ->
            let alternatives = map unLoc (unLoc (mg_alts branches))
            in length alternatives == length constructors && all (forwards alternatives) constructors
    _ -> False
  where
    forwards alternatives constructor = case
        [(fields, args) | match <- alternatives, [pat] <- [m_pats match],
         Just pats <- [constructorPattern constructor pat], Just fields <- [mapM plainPattern pats],
         Just body <- [oneRhs match], Just args <- [call [constructor] body]] of
        [(fields, args)] -> mapM variable args == Just fields
        _ -> False
