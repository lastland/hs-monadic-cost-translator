-- | Follow the value of a written constructor field to the action operand of
-- an ordinary home runner. Field types alone say nothing about execution.
module ConstructionActionFields (ActionFields, actionFields, inModule, sourceActions, typedActions, missingActions) where

import ConstructionActionFlow
import ConstructionAst (atSourceSpan, children, collectAst, typedVariable)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (Role(Primitive), Runtime, roleNames, runtimeName, standardNames)
import Data.Data (Data, cast)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (From)
import GHC.Builtin.Types (consDataCon, tupleDataCon)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConSourceArity)
import GHC.Core.Type (splitTyConApp_maybe)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.SrcLoc (getBufSpan)

data ActionFields = ActionFields
    {sourceActions :: [LHsExpr GhcRn], typedActions :: [LHsExpr GhcTc], missingActions :: [SrcSpan]}

inModule :: Map.Map Module ActionFields -> TypecheckedModule -> ActionFields
inModule fields m = Map.findWithDefault (ActionFields [] [] []) (ms_mod (pm_mod_summary (tm_parsed_module m))) fields

actionFields :: Runtime -> QuickCheck -> [TypecheckedModule] -> Map.Map Module ActionFields
actionFields rt qc modules = Map.fromList [(owner m, selected m fields) | (m, (_, fields)) <- facts]
  where
    owner = ms_mod . pm_mod_summary . tm_parsed_module
    sources = map tm_typechecked_source modules
    -- A source call may use the exported Id while the equation binds its mono Id.
    bindings = concatMap collectAst sources :: [HsBind GhcTc]
    exports = [(getName p, getName m) | s <- sources, ABE p m _ _ <- collectAst s, p /= m]
    aliases = Map.fromList (exports ++ [(getName (unLoc n), target) | FunBind _ n ms <- bindings,
        [L _ Match{m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] rhs)] _}] <- [unLoc (mg_alts ms)],
        Just target <- [alias rhs]])
    alias rhs = case app (unLoc rhs) [] of
        (XExpr (ConLikeTc (RealDataCon con) _ _), []) -> Just (getName con)
        _ -> snd <$> typedVariable rhs
    constructors = Map.fromList [(getName con, con) | s <- sources, RealDataCon con <- collectAst s]
    key = resolve Set.empty
    resolve seen n | Set.notMember n seen, Just next <- Map.lookup n aliases = resolve (Set.insert n seen) next
                   | otherwise = n
    arities = Map.fromList [(getName (unLoc n), matchGroupArity (arguments ms)) | FunBind _ n ms <- bindings]
    facts = [(m, moduleFlow (owner m) key arities constructors rt qc (tm_typechecked_source m)) | m <- modules]
    demanded = executedFields (concatMap (fst . snd) facts)
    selected m fields =
        let renamed = atSourceSpan [(getLocA e, e) | Just (group, _, _, _, _) <- [tm_renamed_source m], e <- collectAst group]
            chosen = [e | (n, e) <- fields, Set.member n demanded]
            matches = [(at, renamed at) | at <- nub (map getLocA chosen)]
        in ActionFields (concatMap snd matches) chosen [at | (at, []) <- matches]

type Facts = ([Flow], [(Node, LHsExpr GhcTc)])

moduleFlow :: Module -> (Name -> Name) -> Map.Map Name Int -> Map.Map Name DataCon -> Runtime -> QuickCheck -> TypecheckedSource -> Facts
moduleFlow owner key arities constructors rt qc = walk True
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
    positional box con xs | length xs == dataConSourceArity con =
            mconcat [pack box (Slot (getName con) i) x | (i, x) <- zip [0..] xs]
                          | otherwise = mempty
    bodies ms = [rhs | L _ m <- unLoc (mg_alts ms), L _ (GRHS _ _ rhs) <- grhssGRHSs (m_grhss m)]
    walk :: Data a => Bool -> a -> Facts
    walk ordinary node
        | Just (FunBind _ n ms) <- cast node :: Maybe (HsBind GhcTc) =
            let f = getName (unLoc n); equations = arguments ms; parameters = matchGroupArity equations > 0
                args = [patternFlow owner (Argument f i) p | L _ m <- unLoc (mg_alts equations), (i, p) <- zip [0..] (m_pats m)]
                result = if parameters then Result f else Binder f
                runner = not (isQuickCheckBinding qc (unLoc n)) && (ordinary || parameters)
                rhss = bodies equations
                roots = [Root x | runner, parameters, rhs <- rhss, action rhs, Just x <- [site rhs]]
            in (roots ++ concat args ++ concatMap (edge result) rhss, []) <> walk runner ms
        | Just PatBind{pat_lhs = pat, pat_rhs = rhs} <- cast node :: Maybe (HsBind GhcTc) =
            ([flow | L _ (GRHS _ _ e) <- grhssGRHSs rhs, Just n <- [site e], flow <- patternFlow owner n pat], []) <>
                walk ordinary rhs
        | otherwise = own <> mconcat (children (walk ordinary) node)
      where
        own | Just e <- cast node = expression ordinary e
            | otherwise = mempty

    expression ordinary e = case site e of
        Nothing -> mempty
        Just n -> case application (unLoc e) [] of
            (XExpr (ConLikeTc (RealDataCon con) _ _), args) -> positional n con args
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
            (HsMultiIf _ branches, []) -> (concat [edge n rhs | L _ (GRHS _ _ rhs) <- branches], [])
            (HsVar _ f, args) | Just con <- Map.lookup (key (getName (unLoc f))) constructors -> positional n con args
            (headExpr, args) | Just (_, f) <- typedVariable (L (getLoc e) headExpr) ->
                let name = key f
                    calls | null args = [From n (Binder name)]
                          | [x] <- args, Just name == standard "id" = edge n x
                          | [x, _] <- args, Just name == standard "const" = edge n x
                          | [_, y] <- args, Just name == standard "seq" = edge n y
                          | Map.lookup name arities == Just (length args) =
                                [Call n name [(i, v) | (i, x) <- zip [0..] args, Just v <- [site x]]]
                          | otherwise = []
                    runs = [Runs n v | ordinary, (i, x) <- zip [0..] args, i `elem` Map.findWithDefault [] name inputs,
                                    action x, Just v <- [site x]] ++
                        [Runs n v | ordinary, [left, right] <- [args],
                            callback <- [right | Just name == standard ">>="] ++ [left | Just name == standard "=<<"],
                            (HsLam _ _ ms, []) <- [app (unLoc callback) []], rhs <- bodies ms, action rhs, Just v <- [site rhs]]
                    -- A first-order packer may receive the action as an
                    -- argument, then store it in a constructor in its body.
                    supplied = [(v, x) | not ordinary, Map.lookup name arities == Just (length args),
                                x <- args, action x, Just v <- [site x]]
                in (calls ++ runs, supplied)
            _ -> mempty

    standard label = lookup ("Prelude." ++ label) (standardNames rt)
    application term args = case app term args of
        (HsVar _ op, f : x : rest) | Just (key (getName (unLoc op))) == standard "$" ->
            application (unLoc f) (x:rest)
        other -> other

    -- Only these concrete combinators take actions to execute. In particular
    -- pure, returning a field, and passing an argument to const are not sinks.
    -- Bind also runs the result of a written continuation; do not infer what
    -- an arbitrary callback might return.
    inputs :: Map.Map Name [Int]
    inputs = Map.fromList ([(n, is) | (label, is) <- [(">>=", [0]), (">>", [0,1]), ("=<<", [1]),
            ("fmap", [1]), ("<$>", [1]), ("<*>", [0,1]), ("<*", [0,1]), ("*>", [0,1])],
            Just n <- [standard label]] ++ [(n, [0]) | n <- roleNames rt Primitive])

app :: HsExpr GhcTc -> [LHsExpr GhcTc] -> (HsExpr GhcTc, [LHsExpr GhcTc])
app (XExpr (WrapExpr (HsWrap _ e))) = app e
app (XExpr (ExpandedThingTc _ e)) = app e
app (HsPar _ e) = app (unLoc e)
app (ExprWithTySig _ e _) = app (unLoc e)
app (HsAppType _ e _) = app (unLoc e)
app (HsApp _ f x) = app (unLoc f) . (x:)
app e = (e,)

-- f = \p -> rhs supplies the same static parameters as f p = rhs.
arguments :: MatchGroup GhcTc (LHsExpr GhcTc) -> MatchGroup GhcTc (LHsExpr GhcTc)
arguments ms | [L _ Match{m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] rhs)] _}] <- unLoc (mg_alts ms),
               (HsLam _ _ actual, []) <- app (unLoc rhs) [] = actual
             | otherwise = ms

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
