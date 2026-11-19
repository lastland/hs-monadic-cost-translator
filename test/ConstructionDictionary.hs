-- | Finite selected dictionary syntax and substitution of field parameters.
module ConstructionDictionary (Evidence(..), Use(..), Environment, trace, renderEvidence,
    instantiateUse, predicateOf, closedFamilyStep, projected, descendant, sameEvidence, sameUse) where

import ConstructionAst (pretty)
import ConstructionHomeRepr (Field(..), HomeReprs, dictionaryBindings)
import ConstructionRuntime (Family, describeName)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classSCSelIds, classTyCon, classTyVars)
import GHC.Core.Coercion (Role(..), coercionLKind, coercionRKind)
import GHC.Core.Coercion.Axiom (toBranchedAxiom)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(..))
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.FamInstEnv (emptyFamInstEnv, reduceTyFamApp_maybe)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon (isClosedSynFamilyTyConWithAxiom_maybe, newTyConCo_maybe, tyConSingleDataCon)
import GHC.Core.Type (getTyVar_maybe, isPredTy, splitTyConApp_maybe, substTyWith)
import GHC.Core.Utils (exprType)
import GHC.Types.Id (isDFunId, realIdUnfolding)
import GHC.Types.Var (isId, isTyVar)

data Evidence = Given Name Type | Instance Id Type [Type] [Evidence] | Super Id Type Evidence | Unknown String
data Use = Use Name Name Family [Type] [Evidence]
type Environment = Map.Map Name C.CoreExpr

trace :: Environment -> Set.Set Name -> Set.Set Name -> C.CoreExpr -> Evidence
trace env given seen expression = case expression of
    C.Tick _ body -> trace env given seen body
    C.Cast body co | Just (_, sel) <- superclassAxiom Unpack co,
            let source = trace env given seen body, Just predicate <- predicateOf source, eqType predicate (coercionLKind co) ->
        Super sel (coercionRKind co) source
    C.Cast body co | familyTransport co, source@Instance{} <- trace env given seen body,
            Just predicate <- predicateOf source, eqType predicate (coercionLKind co) -> source
    C.Cast{} -> Unknown "coerced dictionary"
    _ -> case C.collectArgs expression of
        (C.Var identifier, arguments)
            | isDFunId identifier -> Instance identifier (exprType expression) [ty | C.Type ty <- arguments]
                [trace env given seen arg | arg <- arguments, not (C.isTyCoArg arg), isPredTy (exprType arg)]
            | null arguments, getName identifier `Set.member` given -> Given (getName identifier) (idType identifier)
            | null arguments, getName identifier `Set.notMember` seen, Just rhs <- Map.lookup (getName identifier) env ->
                trace env given (Set.insert (getName identifier) seen) rhs
            | Just cls <- isClassOpId_maybe identifier, identifier `elem` classSCSelIds cls,
              [arg] <- [a | a <- arguments, not (C.isTyCoArg a)] -> Super identifier (exprType expression) (trace env given seen arg)
        _ -> Unknown (pretty expression)

renderEvidence :: Evidence -> String
renderEvidence (Given _ ty) = "given " ++ pretty ty
renderEvidence (Instance name ty _ dependencies) = describeName (getName name) ++ " :: " ++ pretty ty ++
    concatMap ((" {" ++) . (++ "}") . renderEvidence) dependencies
renderEvidence (Super _ ty parent) = "super " ++ pretty ty ++ " of " ++ renderEvidence parent
renderEvidence (Unknown why) = "unresolved " ++ why

predicateOf :: Evidence -> Maybe Type
predicateOf (Super _ predicate _) = Just predicate
predicateOf (Given _ predicate) = Just predicate
predicateOf (Instance _ predicate _ _) = Just predicate
predicateOf Unknown{} = Nothing

substitute :: (Type -> Type) -> [(Name, Evidence)] -> Evidence -> Evidence
substitute ty env ev = case ev of
    Given name t -> case lookup name env of
        Just actual | Just p <- predicateOf actual, eqType (ty t) p -> actual
                    | otherwise -> Unknown "field dictionary predicate mismatch"
        Nothing -> Given name (ty t)
    Instance df p ts ds -> Instance df (ty p) (map ty ts) (map sub ds)
    Super sel p d -> Super sel (ty p) (sub d)
    Unknown{} -> ev
  where sub = substitute ty env

instantiateUse :: Field -> [Type] -> [Evidence] -> Evidence -> Maybe (Use -> Use)
instantiateUse field args ds carrier = do
    (formalClass, formal) <- getClassPredTys_maybe (idType (carrierBinder field))
    (cls, actual) <- predicateOf carrier >>= getClassPredTys_maybe
    vars <- mapM getTyVar_maybe formal
    guard (length vars == length actual && length (parameters field) == length args && length (slots field) == length ds)
    let ty = substTyWith (parameters field ++ vars) (args ++ actual)
        env = (getName (carrierBinder field), carrier) : [(name, d) | (names, d) <- zip (slots field) ds, name <- names]
    guard (cls == formalClass)
    pure (\(Use n c f ts es) -> Use n c f (map ty ts) (map (substitute ty env) es))

data Direction = Pack | Unpack deriving (Eq)

-- A single nominal step is sufficient here and does not chase recursive
-- equations. GHC selects the branch; closed families need no instance env.
closedFamilyStep :: Type -> Maybe Type
closedFamilyStep ty = do
    (tc, args) <- splitTyConApp_maybe ty
    _ <- isClosedSynFamilyTyConWithAxiom_maybe tc
    reductionReducedType <$> reduceTyFamApp_maybe (emptyFamInstEnv, emptyFamInstEnv) Nominal tc args

-- The only transport we peel is a class lifted over the reverse equation.
-- Keep the selected DFun as evidence rather than looking up a replacement.
familyTransport :: Coercion -> Bool
familyTransport co@(TyConAppCo Representational tc [SymCo step@(AxiomInstCo ax _ _)]) =
    case (getClassPredTys_maybe (coercionLKind co), splitTyConApp_maybe (coercionLKind step)) of
        (Just (cls, [_]), Just (family, _)) -> classTyCon cls == tc &&
            isClosedSynFamilyTyConWithAxiom_maybe family == Just ax &&
            maybe False (eqType (coercionRKind step)) (closedFamilyStep (coercionLKind step))
        _ -> False
familyTransport _ = False

-- A class with a single superclass and no methods may have a newtype
-- dictionary. Its own newtype axiom (possibly eta-reduced and applied) is
-- the constructor in reverse and the selector forward. Endpoint equality
-- alone is not a certificate for casts, compositions or arbitrary coercions.
superclassAxiom :: Direction -> Coercion -> Maybe (Class, Id)
superclassAxiom direction co = do
    let (parent, child) = case direction of
            Pack -> (coercionRKind co, coercionLKind co)
            Unpack -> (coercionLKind co, coercionRKind co)
    (cls, arguments) <- getClassPredTys_maybe parent
    [sel] <- pure (classAllSelIds cls)
    [super] <- pure (classSCTheta cls)
    guard (classSCSelIds cls == [sel] && eqType child (substTyWith (classTyVars cls) arguments super))
    axiom <- newTyConCo_maybe (classTyCon cls)
    let match want (SymCo inner) args | want == Pack = match Unpack inner args
        match want (AppCo f x) args = do
            t <- reflexive x
            match want f (t : args)
        match Unpack (AxiomInstCo actual 0 xs) args = do
            ts <- mapM reflexive xs
            guard (actual == toBranchedAxiom axiom && sameList eqType arguments (ts ++ args))
        match _ _ _ = Nothing
        reflexive (Refl t) = Just t
        reflexive (GRefl _ t MRefl) = Just t
        reflexive _ = Nothing
    match direction co []
    pure (cls, sel)

-- Follow the selected superclass field of the actual DFun. Never certify a
-- superclass method merely because the parent instance was registered.
projected :: HomeReprs -> Evidence -> Maybe Evidence
projected homes ev@Super{} = normalize [] ev
  where
    normalize path (Super sel predicate parent) = do
        origin@(Instance df p ts ds) <- normalize path parent
        (cls, _) <- getClassPredTys_maybe p
        guard (sel `elem` classSCSelIds cls && all (progress sel df origin) path)
        let constructor = tyConSingleDataCon (classTyCon cls)
            fromBody rhs = do
                let (bs, body) = C.collectBinders rhs
                    typeBinders = filter isTyVar bs
                guard (length typeBinders == length ts && eqType (substTyWith typeBinders ts (exprType body)) p)
                -- GHC may fuse field transport and newtype packing into
                -- one cast, or bind the transported dictionary separately.
                case body of
                    C.Cast payload (TransCo transport pack) | familyTransport transport,
                        Just (owner, selected) <- superclassAxiom Pack pack, owner == cls && selected == sel,
                        eqType (coercionRKind transport) (coercionLKind pack) ->
                            pure (bs, [C.Cast payload transport])
                    C.Cast payload co | Just (owner, selected) <- superclassAxiom Pack co,
                        owner == cls && selected == sel && eqType (exprType payload) (coercionLKind co) ->
                            pure (bs, [payload])
                    _ -> do
                        (C.Var worker, args) <- pure (C.collectArgs body)
                        guard (worker == dataConWorkId constructor)
                        pure (bs, args)
        (binders, args) <- case Map.lookup (getName df) (dictionaryBindings homes) of
            Just rhs -> fromBody rhs
            Nothing -> case realIdUnfolding df of
                C.DFunUnfolding bs con args | con == constructor -> pure (bs, args)
                C.CoreUnfolding{C.uf_tmpl = rhs} | isNewTyCon (classTyCon cls) -> fromBody rhs
                _ -> Nothing
        let variables = filter isTyVar binders
            formals = filter isId binders
            fields = filter (not . C.isTyCoArg) args
        guard (length variables == length ts && length formals == length ds && length fields == length (classAllSelIds cls))
        field <- lookup sel (zip (classAllSelIds cls) fields)
        let names = map getName formals
            template = trace (dictionaryBindings homes) (Set.fromList names) Set.empty field
        result <- normalize ((sel, df, origin) : path) (substitute (substTyWith variables ts) (zip names ds) template)
        actual <- predicateOf result
        guard (eqType predicate actual || case (getClassPredTys_maybe predicate, getClassPredTys_maybe actual) of
            (Just (wanted, [operand]), Just (found, [ty])) ->
                wanted == found && maybe False (eqType ty) (closedFamilyStep operand)
            _ -> False)
        pure result
    normalize path (Instance df ty ts ds) = Instance df ty ts <$> mapM (normalize path) ds
    normalize _ given@Given{} = Just given
    normalize _ Unknown{} = Nothing
    progress sel df origin (prevSel, prevDf, prev) = sel /= prevSel || df /= prevDf || descendant origin prev
projected _ ev = Just ev

-- Allow another application of the same field only on an actual, strictly
-- nested dictionary from the previous call. Synthetic growing cycles stop.
descendant :: Evidence -> Evidence -> Bool
descendant target (Instance _ _ _ ds) = any (\d -> sameEvidence target d || descendant target d) ds
descendant target (Super _ _ parent) = sameEvidence target parent || descendant target parent
descendant _ _ = False

sameEvidence :: Evidence -> Evidence -> Bool
sameEvidence (Instance a p as ds) (Instance b q bs es) = a == b && eqType p q && sameList eqType as bs && sameList sameEvidence ds es
sameEvidence (Given a p) (Given b q) = a == b && eqType p q
sameEvidence (Super a p d) (Super b q e) = a == b && eqType p q && sameEvidence d e
sameEvidence (Unknown x) (Unknown y) = x == y
sameEvidence _ _ = False

sameList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
sameList f xs ys = length xs == length ys && and (zipWith f xs ys)

sameUse :: Use -> Use -> Bool
sameUse (Use a c f ts ds) (Use b d g us es) = a == b && c == d && f == g && sameList eqType ts us && sameList sameEvidence ds es
