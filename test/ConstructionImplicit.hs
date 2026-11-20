-- | Link explicit implicit parameters to their actual dictionary selections
-- in one checked module. Rebinding the same label introduces a different Id.
module ConstructionImplicit (Implicits(..), implicitParameters, scalarIP) where

import ConstructionAst (atSourceSpan, collectAst, reachable)
import ConstructionRuntime (scalarType)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Predicate (getClassPredTys_maybe, isIPClass)
import GHC.Core.TyCo.Compare (eqType)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Types.Evidence (EvBind(..), EvTerm(..), HsWrapper(..), TcEvBinds(..), evVarsOfTerm)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

data Implicits = Implicits
    { declarations :: [(Name, SrcSpan)]
    , bindingAt :: SrcSpan -> Maybe (Id, LHsExpr GhcTc)
    , readAt :: SrcSpan -> [Name]
    , supplied :: Set.Set Name }

implicitParameters :: TypecheckedModule -> Implicits
implicitParameters checked | null bindings = Implicits [] (const Nothing) (const []) Set.empty
                           | otherwise = Implicits locations (unique typed) selected (Set.union passed uncertain)
  where
    source = tm_typechecked_source checked
    bindings = collectAst source :: [IPBind GhcTc]
    locations = [(getName v, getLocA label) | IPBind v label _ <- bindings]
    roots = Set.fromList (map fst locations)
    typed = atSourceSpan [(getLocA e, (v, e)) | IPBind v _ e <- bindings]
    unique index at | Strict.Just _ <- getBufSpan at, [x] <- index at = Just x
                    | otherwise = Nothing
    -- A source ?read is a typed projection from a particular evidence Id.
    projections = [(getLocA e, getName v, eqType ty (lhsExprType e)) |
        e@(L _ (XExpr (WrapExpr (HsWrap _ (HsVar _ n))))) <- collectAst source, let v = unLoc n, Just ty <- [payload v]]
    projected = atSourceSpan [(at, (v, same)) | (at, v, same) <- projections]
    tokens = Set.fromList [p | Just (group, _, _, _, _) <- [tm_renamed_source checked],
        e@(L _ HsIPVar{}) <- collectAst group :: [LHsExpr GhcRn], Strict.Just p <- [getBufSpan (getLocA e)]]
    selected at | Strict.Just p <- getBufSpan at, Set.member p tokens, Just (v, True) <- unique projected at,
        [n] <- Set.toList (origins aliases v) = [n]
                | otherwise = []
    wrappers = concatMap flatten (collectAst source)
    flatten (WpCompose a b) = flatten a ++ flatten b
    flatten (WpFun a b _) = flatten a ++ flatten b
    flatten w = [w]
    evidence = [b | EvBinds bs <- collectAst source ++ [ev | WpLet ev <- wrappers], b <- toList bs]
    aliases = Map.fromListWith (++) [(getName v, [getName a]) | EvBind v (EvExpr (C.Var a)) _ <- evidence]
    origins edges n = Set.intersection roots (reachable edges (Set.singleton n))
    -- Passing evidence to a callee is distinct from reading a scalar at the
    -- property site. Follow dependencies even if that argument packs dictionaries.
    vars = map getName . nonDetEltsUniqSet . evVarsOfTerm
    deps = Map.fromListWith (++) [(getName (eb_lhs b), vars (eb_rhs b)) | b <- evidence]
    passed = Set.intersection roots (reachable deps (Set.fromList [n | WpEvApp term <- wrappers, n <- vars term]))
    -- An ambiguous token or a non-alias evidence path cannot prove exclusive
    -- use, even when some other read of the same dictionary was resolved.
    uncertain = Set.unions [Set.difference (origins deps v) (Set.fromList (selected at)) | (at, v, _) <- projections]

payload :: Id -> Maybe Type
payload v | Just (cls, [_, ty]) <- getClassPredTys_maybe (idType v), isIPClass cls = Just ty
          | otherwise = Nothing

scalarIP :: Id -> Bool
scalarIP = maybe False scalarType . payload
