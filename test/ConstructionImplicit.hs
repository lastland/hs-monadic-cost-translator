-- | Link explicit implicit parameters to their actual dictionary selections
-- in one checked module. Rebinding the same label introduces a different Id.
module ConstructionImplicit (Implicits(..), Reference(..), implicitFormals, implicitParameters, scalarIP) where

import ConstructionAst (atSourceSpan, children, collectAst, firstOrderCall, reachable, variable)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (scalarType)
import Data.Data (Data, cast)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Predicate (getClassPredTys_maybe, isIPClass)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (isPiTy, splitFunTys)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Types.Evidence (EvBind(..), EvTerm(..), HsWrapper(..), TcEvBinds(..), evVarsOfTerm)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.SrcLoc (BufSpan, getBufSpan)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

data Implicits = Implicits
    { declarations :: [(Name, SrcSpan)]
    , bindingAt :: SrcSpan -> Maybe (Id, LHsExpr GhcTc)
    , references :: LHsExpr GhcRn -> [Reference]
    , dictionaryIds :: Set.Set Name, ordinaryIPs :: Set.Set Name }

-- A call reads its actual IP only when this particular formal is read.
data Reference = Direct Name | AtCall Name Name

type Formals = Map.Map Name (Int, [Id])

implicitFormals :: QuickCheck -> [TypecheckedModule] -> Formals
implicitFormals qc = Map.unions . map signatures
  where
    signatures m = Map.fromList [(getName v, (length args, ds)) | (v, vs) <- methods ++ exports,
        isQuickCheckBinding qc v, let (_, theta, rho) = tcSplitSigmaTy (idType v); (args, _) = splitFunTys rho,
        not (any (isPiTy . scaledThing) args), let ds = [d | d <- vs, scalarIP d, any (eqType (idType d)) theta], not (null ds)]
      where
        source = tm_typechecked_source m
        methods = [(unLoc n, parameters w) | FunBind (w, _) n _ <- collectAst source :: [HsBind GhcTc], Strict.Just _ <- [getBufSpan (getLocA n)]]
        impls = Map.fromList methods
        exports = [(poly, abs_ev_vars ab ++ ds) | ab <- collectAst source, ABE poly mono WpHole _ <- abs_exports ab,
            Just ds <- [Map.lookup mono impls]]
    parameters (WpCompose a b) = parameters a ++ parameters b
    parameters (WpEvLam v) = [v]
    parameters _ = []

flatten :: HsWrapper -> [HsWrapper]
flatten (WpCompose a b) = flatten a ++ flatten b
flatten (WpFun a b t) = WpFun a b t : flatten a ++ flatten b
flatten w = [w]

type CallSite = (BufSpan, Name)

-- Keep each evidence application attached to the named typed occurrence that
-- supplied it. Non-variable expressions and adapters give no call certificate.
applications :: Data a => a -> [(Maybe CallSite, [EvTerm])]
applications node
    | Just e@(L _ (XExpr (WrapExpr (HsWrap w (HsVar _ n))))) <- cast node :: Maybe (LHsExpr GhcTc) =
        let ws = flatten w; key = case getBufSpan (getLocA e) of
                Strict.Just p | all direct ws -> Just (p, getName (unLoc n))
                _ -> Nothing
        in [(key, [term | WpEvApp term <- ws])]
    | Just w <- cast node = [(Nothing, [term | WpEvApp term <- flatten w])]
    | otherwise = concat (children applications node)
  where
    direct WpEvApp{} = True
    direct WpTyApp{} = True
    direct WpHole = True
    direct _ = False

implicitParameters :: Formals -> TypecheckedModule -> Implicits
implicitParameters formals checked
    | Set.null roots = Implicits [] (const Nothing) (const []) Set.empty Set.empty
    | otherwise = Implicits locations (unique typed) mentions roots (Set.union passed uncertain)
  where
    source = tm_typechecked_source checked
    bindings = collectAst source :: [IPBind GhcTc]
    locations = [(getName v, getLocA label) | IPBind v label _ <- bindings]
    formalIds = [v | WpEvLam v <- wrappers, scalarIP v] ++ [v | ab <- collectAst source, v <- abs_ev_vars ab, scalarIP v]
    roots = Set.fromList (map fst locations ++ map getName formalIds)
    typed = atSourceSpan [(getLocA e, (v, e)) | IPBind v _ e <- bindings]
    unique index at | Strict.Just _ <- getBufSpan at, [x] <- index at = Just x
                    | otherwise = Nothing
    -- A source ?read is a typed projection from a particular evidence Id.
    projections = [(getLocA e, getName v, eqType ty (lhsExprType e)) |
        e@(L _ (XExpr (WrapExpr (HsWrap _ (HsVar _ n))))) <- collectAst source, let v = unLoc n, Just ty <- [payload v]]
    projected = atSourceSpan [(at, (v, same)) | (at, v, same) <- projections]
    groups = [group | Just (group, _, _, _, _) <- [tm_renamed_source checked]]
    tokens = Set.fromList [p | group <- groups,
        e@(L _ HsIPVar{}) <- collectAst group :: [LHsExpr GhcRn], Strict.Just p <- [getBufSpan (getLocA e)]]
    selected at | Strict.Just p <- getBufSpan at, Set.member p tokens, Just (v, True) <- unique projected at,
        [n] <- Set.toList (origins aliases v) = [n]
                | otherwise = []
    wrappers = concatMap flatten (collectAst source)
    evidence = [b | EvBinds bs <- collectAst source ++ [ev | WpLet ev <- wrappers], b <- toList bs]
    aliases = Map.fromListWith (++) [(getName v, [getName a]) | EvBind v (EvExpr (C.Var a)) _ <- evidence]
    origins edges n = Set.intersection roots (reachable edges (Set.singleton n))
    -- Passing evidence to a callee is distinct from reading a scalar at the
    -- property site. Follow dependencies even if that argument packs dictionaries.
    vars = map getName . nonDetEltsUniqSet . evVarsOfTerm
    deps = Map.fromListWith (++) [(getName (eb_lhs b), vars (eb_rhs b)) | b <- evidence]
    calls = applications source
    counts = Map.fromListWith (+) [(key, 1 :: Int) | (Just key, _) <- calls]
    saturated = Set.fromList [(p, name) | group <- groups, e <- collectAst group, let (h, xs) = firstOrderCall e,
        Just name <- [variable h], Just (arity, _) <- [Map.lookup name formals], length xs == arity,
        Strict.Just p <- [getBufSpan (getLocA h)]]
    transfer (Just key@(_, fn)) (EvExpr (C.Var actual))
        | Set.member key saturated, Map.lookup key counts == Just 1, Just (_, ds) <- Map.lookup fn formals,
          [formal] <- filter (eqType (idType actual) . idType) ds, [origin] <- Set.toList (origins aliases (getName actual)),
          origins deps (getName actual) == Set.singleton origin = Just (key, AtCall (getName formal) origin)
    transfer _ _ = Nothing
    uses = [(transfer key term, term) | (key, terms) <- calls, term <- terms]
    transfers = Map.fromListWith (++) [(key, [ref]) | (Just (key, ref), _) <- uses]
    passed = Set.intersection roots (reachable deps (Set.fromList [n | (Nothing, term) <- uses, n <- vars term]))
    mentions e@(L _ HsIPVar{}) = map Direct (selected (getLocA e))
    mentions e@(L _ (HsVar _ n)) | Strict.Just p <- getBufSpan (getLocA e) = Map.findWithDefault [] (p, unLoc n) transfers
    mentions _ = []
    -- An ambiguous token or a non-alias evidence path cannot prove exclusive
    -- use, even when some other read of the same dictionary was resolved.
    uncertain = Set.unions [Set.difference (origins deps v) (Set.fromList (selected at)) | (at, v, _) <- projections]

payload :: Id -> Maybe Type
payload v | Just (cls, [_, ty]) <- getClassPredTys_maybe (idType v), isIPClass cls = Just ty
          | otherwise = Nothing

scalarIP :: Id -> Bool
scalarIP = maybe False scalarType . payload
