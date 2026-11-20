-- | Follow the value of a written constructor field to the action operand of
-- an ordinary home runner. Field types alone say nothing about execution.
module ConstructionActionFields (ActionFields, actionFields, inModule, sourceActions, typedActions) where

import ConstructionAst (atSourceSpan, children, collectAst, typedVariable)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (Role(Primitive), Runtime, roleNames, runtimeName, standardNames)
import Data.Data (Data, cast)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (From)
import GHC.Builtin.Types (consDataCon, tupleDataCon)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.Type (splitTyConApp_maybe)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.SrcLoc (BufSpan, getBufSpan)

data ActionFields = ActionFields {sourceActions :: [LHsExpr GhcRn], typedActions :: [LHsExpr GhcTc]}

inModule :: Map.Map Module ActionFields -> TypecheckedModule -> ActionFields
inModule fields m = Map.findWithDefault (ActionFields [] []) (ms_mod (pm_mod_summary (tm_parsed_module m))) fields

data Node = Binder Name | Argument Name Int | Result Name | Written Module BufSpan | Tail Node Int | Item Node Int deriving (Eq, Ord)
data Slot = Slot Name Int deriving (Eq, Ord)
data Flow = From Node Node | Pack Node Slot Node | Project Node Slot Node | Run Node

-- A projection creates a value edge only for a matching constructor and slot.
-- The graph is finite even for recursive calls and nested recursive datatypes:
-- closing it adds edges between existing syntax nodes, never new field paths.
executed :: [Flow] -> Set.Set Node
executed flows = close initial
  where
    index entries = Map.fromListWith Set.union [(k, Set.singleton v) | (k, v) <- entries]
    initial = index [(to, from) | From to from <- flows]
    fields = index [((box, slot), field) | Pack box slot field <- flows]
    projections = [(to, slot, box) | Project to slot box <- flows]
    closure graph = go Set.empty
      where
        go seen [] = seen
        go seen (n:ns) | Set.member n seen = go seen ns
                       | otherwise = go (Set.insert n seen) (Set.toList (Map.findWithDefault Set.empty n graph) ++ ns)
    close graph =
        let producers = Map.fromList [(box, closure graph [box]) | (_, _, box) <- projections]
            more = index [(to, field) | (to, slot, box) <- projections, con <- Set.toList (producers Map.! box),
                field <- Set.toList (Map.findWithDefault Set.empty (con, slot) fields)]
            next = Map.unionWith Set.union graph more
        in if next == graph then closure graph [n | Run n <- flows] else close next

actionFields :: Runtime -> QuickCheck -> [TypecheckedModule] -> Map.Map Module ActionFields
actionFields rt qc modules = Map.fromList [(owner m, selected m fields) | (m, (_, fields)) <- facts]
  where
    owner = ms_mod . pm_mod_summary . tm_parsed_module
    sources = map tm_typechecked_source modules
    -- A source call may use the exported Id while the equation binds its mono Id.
    exports = Map.fromList [(getName p, getName m) | s <- sources, ABE p m _ _ <- collectAst s]
    key n = Map.findWithDefault n n exports
    arities = Map.fromList [(getName (unLoc n), matchGroupArity ms) | s <- sources, FunBind _ n ms <- collectAst s :: [HsBind GhcTc]]
    facts = [(m, moduleFlow (owner m) key arities rt qc (tm_typechecked_source m)) | m <- modules]
    demanded = executed (concatMap (fst . snd) facts)
    selected m fields =
        let renamed = atSourceSpan [(getLocA e, e) | Just (group, _, _, _, _) <- [tm_renamed_source m], e <- collectAst group]
            chosen = nubBy (\a b -> getLocA a == getLocA b) [e | (n, e) <- fields, Set.member n demanded]
        in ActionFields (concatMap (renamed . getLocA) chosen) chosen

type Facts = ([Flow], [(Node, LHsExpr GhcTc)])

moduleFlow :: Module -> (Name -> Name) -> Map.Map Name Int -> Runtime -> QuickCheck -> TypecheckedSource -> Facts
moduleFlow owner key arities rt qc = walk False
  where
    site e = case getBufSpan (getLocA e) of
        Strict.Just bytes -> Just (Written owner bytes)
        Strict.Nothing -> Nothing
    edge to from = [From to n | Just n <- [site from]]
    action e = case splitTyConApp_maybe (lhsExprType e) of
        Just (tc, _) -> getName tc == runtimeName rt "Memorist.Base.ANT" "MemoristT"
        Nothing -> False
    pack box slot e = case site e of
        Just n -> ([Pack box slot n], [(n, e) | action e])
        Nothing -> mempty
    bodies ms = [rhs | L _ m <- unLoc (mg_alts ms), L _ (GRHS _ _ rhs) <- grhssGRHSs (m_grhss m)]
    walk :: Data a => Bool -> a -> Facts
    walk ordinary node
        | Just (FunBind _ n ms) <- cast node :: Maybe (HsBind GhcTc) =
            let f = getName (unLoc n)
                args = [patternFlow owner (Argument f i) p | L _ m <- unLoc (mg_alts ms), (i, p) <- zip [0..] (m_pats m)]
                result = if matchGroupArity ms == 0 then Binder f else Result f
                runner = not (isQuickCheckBinding qc (unLoc n)) && (ordinary || matchGroupArity ms > 0)
            in (concat args ++ concatMap (edge result) (bodies ms), []) <> walk runner ms
        | Just PatBind{pat_lhs = pat, pat_rhs = rhs} <- cast node :: Maybe (HsBind GhcTc) =
            ([flow | L _ (GRHS _ _ e) <- grhssGRHSs rhs, Just n <- [site e], flow <- patternFlow owner n pat], []) <>
                walk ordinary rhs
        | otherwise = own <> mconcat (children (walk ordinary) node)
      where
        own | Just e <- cast node = expression ordinary e
            | otherwise = mempty

    expression ordinary e = case site e of
        Nothing -> mempty
        Just n -> case app (unLoc e) [] of
            (XExpr (ConLikeTc (RealDataCon con) _ _), args) ->
                mconcat [pack n (Slot (getName con) i) x | (i, x) <- zip [0..] args]
            (RecordCon{rcon_con = L _ (RealDataCon con), rcon_flds = record}, []) ->
                mconcat [pack n slot x | (slot, x) <- recordSlots con record]
            (ExplicitTuple _ xs box, []) ->
                mconcat [pack n (Slot (getName (tupleDataCon box (length xs))) i) x | (i, Present _ x) <- zip [0..] xs]
            (ExplicitList _ xs, []) -> mconcat [let cons = getName consDataCon; cell 0 = n; cell k = Tail n k
                in ([Pack (cell i) (Slot cons 1) (cell (i+1))], []) <> pack (cell i) (Slot cons 0) x | (i, x) <- zip [0..] xs]
            (HsCase _ input ms, []) -> (concatMap (edge n) (bodies ms) ++
                [flow | L _ m <- unLoc (mg_alts ms), [p] <- [m_pats m], Just v <- [site input], flow <- patternFlow owner v p], [])
            (HsLet _ _ body, []) -> (edge n body, [])
            (HsIf _ _ x y, []) -> (edge n x ++ edge n y, [])
            (headExpr, args) | Just (_, f) <- typedVariable (L (getLoc e) headExpr) ->
                let name = key f
                    calls | null args = [From n (Binder name)]
                          | Map.lookup name arities == Just (length args) =
                                From n (Result name) : concat [edge (Argument name i) x | (i, x) <- zip [0..] args]
                          | otherwise = []
                    runs = [Run v | ordinary, (i, x) <- zip [0..] args, i `elem` Map.findWithDefault [] f inputs,
                                    action x, Just v <- [site x]]
                in (calls ++ runs, [])
            _ -> mempty

    -- Only these concrete combinators take actions to execute. In particular
    -- pure, returning a field, and passing an argument to const are not sinks.
    inputs :: Map.Map Name [Int]
    inputs = Map.fromList ([(n, is) | (label, is) <- [(">>=", [0]), (">>", [0,1]), ("=<<", [1]),
            ("fmap", [1]), ("<$>", [1]), ("<*>", [0,1]), ("<*", [0,1]), ("*>", [0,1])],
            Just n <- [lookup ("Prelude." ++ label) (standardNames rt)]] ++ [(n, [0]) | n <- roleNames rt Primitive])

app :: HsExpr GhcTc -> [LHsExpr GhcTc] -> (HsExpr GhcTc, [LHsExpr GhcTc])
app (XExpr (WrapExpr (HsWrap _ e))) = app e
app (XExpr (ExpandedThingTc _ e)) = app e
app (HsPar _ e) = app (unLoc e)
app (ExprWithTySig _ e _) = app (unLoc e)
app (HsAppType _ e _) = app (unLoc e)
app (HsApp _ f x) = app (unLoc f) . (x:)
app e = (e,)

recordSlots :: DataCon -> HsRecFields GhcTc a -> [(Slot, a)]
recordSlots con fields = [(Slot (getName con) i, hfbRHS field) | L _ field <- rec_flds fields,
    Just i <- [lookup (getName (foExt (unLoc (hfbLHS field)))) indices]]
  where indices = zip (map flSelector (dataConFieldLabels con)) [0..]

patternFlow :: Module -> Node -> LPat GhcTc -> [Flow]
patternFlow owner = go
  where
    go n (L l p) = case p of
        VarPat _ v -> [From (Binder (getName (unLoc v))) n]
        AsPat _ v inner -> From (Binder (getName (unLoc v))) n : go n inner
        ParPat _ inner -> go n inner
        SigPat _ inner _ -> go n inner
        BangPat _ inner -> go n inner
        LazyPat _ inner -> go n inner
        XPat (CoPat _ inner _) -> go n (L l inner)
        ConPat _ (L _ (RealDataCon con)) ps -> case ps of
            PrefixCon _ xs -> positional con xs
            InfixCon x y -> positional con [x, y]
            RecCon fields -> concatMap field (recordSlots con fields)
        TuplePat _ xs box -> positional (tupleDataCon box (length xs)) xs
        ListPat _ xs -> concat [go (at i) x | (i,x) <- zip [0..] xs] ++
            concat [[Project (at i) (Slot cons 0) (cell i), Project (cell (i+1)) (Slot cons 1) (cell i)] | i <- [0..length xs-1]]
          where cons = getName consDataCon; cell 0 = n; cell i = Tail n i; at = Item n
        _ -> []
      where
        positional con xs = concatMap field (zipWith (\i x -> (Slot (getName con) i, x)) [0..] xs)
        field (s, x) = case getBufSpan (getLocA x) of
            Strict.Just bytes -> let next = Written owner bytes in Project next s n : go next x
            Strict.Nothing -> []
