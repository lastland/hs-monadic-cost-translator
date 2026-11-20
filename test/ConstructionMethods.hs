-- | Finite home methods whose result is the audited computation carrier.
-- A default's self is conditional only in its own retained class equation.
module ConstructionMethods (Methods, homeMethods, retainedMethods, defaultClass, conditionalMethod, selectedMethod) where

import ConstructionAst (allBindings, collectAst)
import ConstructionDictionary
import ConstructionHomeRepr (Field(..))
import ConstructionRuntime (Runtime, isMemoristType)
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classOpItems, classTyCon, classTyVars)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, splitAppTy_maybe, splitForAllTyCoVar_maybe, splitFunTy_maybe, substTyWith)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Core.Utils (exprType)
import GHC.Tc.Types (tcg_tcs)
import GHC.Types.Var (isId, isTyVar)
import GHC.Types.SrcLoc (isRealSubspanOf)

data Default = Default Name Name Field SrcSpan [Use]
data Methods = Methods Environment (Map.Map Name [Use]) [Default]

homeMethods :: Runtime -> TypecheckedModule -> Environment -> [(Id, C.CoreExpr)]
    -> (C.CoreExpr -> [Use]) -> Methods
homeMethods rt checked env raw collect = Methods env bodies defaults
  where
    bodies = Map.fromList [(getName v, collect rhs) | (v, rhs) <- allBindings [C.Rec raw]]
    equations = [(unLoc n, getLocA binding) |
        Just (group, _, _, _, _) <- [tm_renamed_source checked],
        ClassDecl{tcdMeths = methods} <- collectAst group :: [TyClDecl GhcRn],
        binding@(L _ (FunBind _ n _)) <- toList methods]
    defaults = [Default dm (getName cls) (Field dm methodVars [] self) at (collect rhs) |
        tc <- tcg_tcs (fst (tm_internals_ checked)), Just cls <- [tyConClass_maybe tc],
        (selector, Just (dm, _)) <- classOpItems cls, computation rt (idType selector),
        Just at <- [lookup (getName selector) equations], Just rhs <- [Map.lookup dm env],
        let binders = fst (C.collectBinders rhs),
        [self] <- [filter (\v -> isId v && isPredTy (idType v)) binders],
        Just (actual, _) <- [getClassPredTys_maybe (idType self)], actual == cls,
        let methodVars = drop (length (classTyVars cls)) (filter isTyVar binders)]

-- Resolve a default by actual class metadata, never by its occurrence spelling.
defaultClass :: Id -> Maybe Name
defaultClass identifier = do
    cls <- firstClass (idType identifier)
    guard (any (\(_, dm) -> maybe False ((== getName identifier) . fst) dm) (classOpItems cls))
    pure (getName cls)
  where
    firstClass ty
        | Just (_, rest) <- splitForAllTyCoVar_maybe ty = firstClass rest
        | Just (_, _, argument, _) <- splitFunTy_maybe ty = fst <$> getClassPredTys_maybe argument
        | otherwise = Nothing

computation :: Runtime -> Type -> Bool
computation rt ty
    | Just (_, rest) <- splitForAllTyCoVar_maybe ty = computation rt rest
    | Just (_, _, _, rest) <- splitFunTy_maybe ty = computation rt rest
    | Just (carrier, _) <- splitAppTy_maybe ty = isMemoristType rt carrier
    | otherwise = False

conditionalMethod :: Runtime -> [Methods] -> Either Name SrcSpan -> Use -> Bool
conditionalMethod rt modules site (Use selected cls _ _ [Given binder predicate]) =
    any owns [d | Methods _ _ ds <- modules, d <- ds]
  where
    owns (Default dm owner field at _) =
        owner == cls && binder == getName (carrierBinder field) &&
        eqType predicate (idType (carrierBinder field)) &&
        (case site of Left name -> name == dm; Right span' -> contains at span') &&
        case getClassPredTys_maybe predicate of
            Just (actual, _) -> any (\(selector, _) -> getName selector == selected &&
                computation rt (idType selector)) (classOpItems actual)
            _ -> False
    contains (RealSrcSpan outer _) (RealSrcSpan inner _) = isRealSubspanOf inner outer
    contains _ _ = False
conditionalMethod _ _ _ _ = False

-- Add retained, possibly erased method occurrences to the owning default.
-- Ownership still requires its exact self Id when that obligation is checked.
retainedMethods :: [(Either Name SrcSpan, Use)] -> Methods -> Methods
retainedMethods sites (Methods env bodies defaults) = Methods env bodies
    [Default dm cls field at (obligations ++ [u | (site, u) <- sites,
        belongs dm at site]) |
        Default dm cls field at obligations <- defaults]
  where
    belongs dm _ (Left name) = dm == name
    belongs _ (RealSrcSpan outer _) (Right (RealSrcSpan inner _)) = isRealSubspanOf inner outer
    belongs _ _ _ = False

-- Return one complete selected implementation obligation. A repeated edge is
-- rejected by the caller: this finite path is not a higher-order evaluator.
selectedMethod :: Runtime -> [Methods] -> Use -> Maybe (Name, [Use])
selectedMethod rt modules (Use selected expected family types [evidence@(Instance df predicate args [])]) = do
    (cls, classArgs) <- getClassPredTys_maybe predicate
    guard (getName cls == expected && length classArgs <= length types &&
        and (zipWith eqType classArgs types))
    (selector, dm) <- unique [(sel, def) | (sel, def) <- classOpItems cls,
        getName sel == selected || maybe False ((== selected) . fst) def]
    guard (computation rt (idType selector))
    dictionary <- Map.lookup (getName df) environment
    let (binders, body) = C.collectBinders dictionary
        variables = filter isTyVar binders
        ty = substTyWith variables args
    guard (all isTyVar binders && length variables == length args && eqType (ty (exprType body)) predicate)
    (C.Var worker, constructorArgs) <- pure (C.collectArgs body)
    guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)))
    let fields = filter (not . C.isTyCoArg) constructorArgs
    guard (length fields == length (classAllSelIds cls))
    field <- lookup selector (zip (classAllSelIds cls) fields)
    (C.Var method, supplied) <- pure (C.collectArgs field)
    guard (all C.isTypeArg supplied)
    rhs <- Map.lookup (getName method) environment
    let (methodBinders, _) = C.collectBinders rhs
        suppliedTypes = [ty t | C.Type t <- supplied]
        methodTypes = drop (length classArgs) types
        actualTypes = suppliedTypes ++ methodTypes
    guard (length actualTypes == length (filter isTyVar methodBinders))
    if selected == getName selector then do
        obligations <- Map.lookup (getName method) bodies
        instantiated <- instantiateTypes (filter isTyVar methodBinders) actualTypes obligations
        pure (getName method, instantiated)
    else do
        (defaultName, _) <- dm
        -- A direct default use must really be the selected inherited field,
        -- not a sibling or an explicit override that happens to call it.
        (C.Var actual, callArgs) <- pure (C.collectArgs (stripEvidence (snd (C.collectBinders rhs))))
        guard (getName actual == defaultName)
        let givens = Set.fromList [getName v | v <- methodBinders, isId v, isPredTy (idType v)]
            actualEvidence = [trace environment givens Set.empty e | e <- callArgs,
                not (C.isTyCoArg e), isPredTy (exprType e)]
        templateEvidence <- instantiateTypes (filter isTyVar methodBinders) actualTypes
            [Use selected expected family [] actualEvidence]
        guard (case templateEvidence of [Use _ _ _ _ [ev]] -> sameEvidence ev evidence; _ -> False)
        Default _ _ scope _ obligations <- unique [d | Methods _ _ ds <- modules,
            d@(Default name owner _ _ _) <- ds, name == defaultName, owner == expected]
        apply <- instantiateUse scope methodTypes [] evidence
        pure (defaultName, map apply obligations)
  where
    environment = Map.unions [env | Methods env _ _ <- modules]
    bodies = Map.unions [bs | Methods _ bs _ <- modules]
    unique [x] = Just x
    unique _ = Nothing
    stripEvidence (C.Let (C.NonRec binder _) body) | isPredTy (idType binder) = stripEvidence body
    stripEvidence body = body
selectedMethod _ _ _ = Nothing
