-- Inspect the translated allocation itself: unrelated support declarations in
-- the complete module can neither satisfy nor mask this check.
module ConstructionAllocation (checkPhantomAllocation) where

import ConstructionAst
import ConstructionRuntime (resolveExport, resolveType)
import Data.Maybe (isJust)
import GHC

data AllocationNames = AllocationNames
    { allocationProducer, allocationLazy, allocationView, allocationPair, allocationPure :: Name
    , allocationConstructors :: [Name]
    }

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
    let names = AllocationNames
            { allocationProducer = producer, allocationLazy = allocate, allocationView = view
            , allocationPair = pair, allocationPure = ret, allocationConstructors = map getName (tyConDataCons parcel)
            }
        inspect name = do
            ownerName <- resolveExport owner name
            [binding] <- pure [b | b <- collectAst group, bindingName b == Just ownerName]
            if singleRoot names binding then pure () else
                fail (name ++ " must suspend one producer invocation and return both instantiations of that root with every original field thunk")
    mapM_ inspect ["viewsM", "inferredM"]

singleRoot :: AllocationNames -> HsBind GhcRn -> Bool
singleRoot names owner = case allocators of
    [allocator] | uses (allocationProducer names) == 1, uses allocator == 1, [root] <- roots allocator,
                  [alias] <- aliases (fst root) -> returnsPair alias (snd root)
    _ -> False
  where
    bindings = collectAst owner
    uses target = length [() | HsVar _ name <- collectAst owner :: [HsExpr GhcRn], unLoc name == target]
    allocators = [name | binding <- bindings, Just name <- [bindingName binding],
        Just [([parameter], body)] <- [equations binding], Just input <- [plainPattern parameter],
        Just [action] <- [call [allocationLazy names] body], retagsResult names input action]
    roots allocator = [(root, terminal) |
        L _ (HsDo _ (DoExpr Nothing) (L _ statements)) <- collectAst owner :: [LHsExpr GhcRn],
        L _ (LastStmt _ terminal _ _) : _ <- [reverse statements],
        L _ (BindStmt _ pat rhs) <- statements,
        Just root <- [plainPattern pat], Just [action] <- [call [allocator] rhs], returnsProducer action]
    aliases root = [alias | binding <- bindings, Just alias <- [bindingName binding], Just [([], rhs)] <- [equations binding],
        Just [_, input] <- [call [allocationView names] rhs], variable input == Just root]
    returnsPair alias expression = case spine expression of
        (L _ (HsLet _ _ body), []) -> returnsPair alias body
        (L _ (HsDo _ (DoExpr Nothing) (L _ statements)), [])
            | L _ (LastStmt _ terminal _ _) : _ <- reverse statements -> returnsPair alias terminal
        _ | Just [value] <- call [allocationPure names] expression,
            Just [a,b] <- call [allocationPair names] value ->
                variable a == Just alias && variable b == Just alias
        _ -> False
    returnsProducer expression = case spine expression of
        (L _ (HsDo _ (DoExpr Nothing) (L _ statements)), [])
            | L _ (LastStmt _ terminal _ _) : _ <- reverse statements -> returnsProducer terminal
        _ -> isJust (call [allocationProducer names] expression)

retagsResult :: AllocationNames -> Name -> LHsExpr GhcRn -> Bool
retagsResult names producerArgument action = case spine action of
    (L _ (HsDo _ (DoExpr Nothing) (L _ [L _ (BindStmt _ pat rhs), L _ (LastStmt _ end _ _)])), [])
        | variable rhs == Just producerArgument, Just result <- plainPattern pat,
          Just [value] <- call [allocationPure names] end, (L _ (HsCase _ scrutinee branches), []) <- spine value,
          variable scrutinee == Just result ->
            let alternatives = map unLoc (unLoc (mg_alts branches))
                constructors = allocationConstructors names
            in length alternatives == length constructors && all (forwards alternatives) constructors
    _ -> False
  where
    forwards alternatives constructor = case
        [(fields, args) | match <- alternatives, [pat] <- [m_pats match],
         Just pats <- [constructorPattern constructor pat], Just fields <- [mapM plainPattern pats],
         Just body <- [oneRhs match], Just args <- [call [constructor] body]] of
        [(fields, args)] -> mapM variable args == Just fields
        _ -> False
