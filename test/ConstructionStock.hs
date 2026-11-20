-- | Conditional declaration proofs and exact concrete stock-field replay.
module ConstructionStock (stockUse) where

import ConstructionDemandMethods
import ConstructionDictionary
import ConstructionHomeRepr (HomeReprs)
import ConstructionMethods (MethodSite(..))
import ConstructionRuntime hiding (runtime)
import ConstructionSuper (validSuper)
import Control.Monad (guard)
import Data.List (deleteFirstsBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, splitTyConApp_maybe, substTyWith)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Core.Utils (exprType)
import GHC.Types.Var (isId, isTyVar)

stockUse :: Runtime -> HomeReprs -> [StockMethods] -> MethodSite -> (Use -> Bool) -> Use -> Bool
stockUse rt homes programs origin tag use =
    case [slots | Stock _ _ _ _ ms <- stocks, StockMethod _ _ impl _ slots _ _ <- ms,
            MethodBinding owner <- [origin], owner == getName impl] of
        [slots] -> prove [] slots use
        [] | homeEntry use -> prove [] [] use
        _ -> False
  where
    stocks = concat [ss | StockMethods _ ss <- programs]
    env = Map.unions [e | StockMethods e _ <- programs]
    homeEntry (Use _ _ ScalarMethod _ [e]) = case resolve [] e of
        Just (Instance df predicate _ ds) -> stockOwner df ||
            (runtimeOwns rt (getName df) &&
             case (getClassPredTys_maybe predicate, ds) of
                (Just (_, [operand]), [child]) -> case splitTyConApp_maybe operand of
                    Just (tc, [payload]) -> getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD" &&
                        (scalarType payload || case child of Instance owner _ _ _ -> stockOwner owner; _ -> False)
                    _ -> False
                _ -> False)
        _ -> False
    homeEntry _ = False
    stockOwner df = any (\(Stock owner _ _ _ _) -> df == owner) stocks
    prove _ _ obligation | tag obligation = True
    prove active own (Use method expected ScalarMethod [operand] [e])
        | not (isProtected rt operand), Just ev <- resolve [] e,
          Just (cls, [actual]) <- predicateOf ev >>= getClassPredTys_maybe,
          getName cls == expected, eqType actual operand =
            (scalarType operand && ordinary ev) ||
            (standardMethod rt method cls &&
                (conditional own ev || runtime active own method ev || selected active own method ev))
    prove _ _ _ = False
    conditional own (Given name predicate) = any (\v -> getName v == name && eqType (idType v) predicate) own
    conditional own (Transport _ parent) = conditional own parent
    conditional own (Super sel predicate parent) = validSuper sel predicate parent && conditional own parent
    conditional _ _ = False
    -- Even an inert ordinary-call context needs a fully selected origin.
    contextOrigin own ev@Given{} = conditional own ev
    contextOrigin own (Instance _ _ _ children) = all (contextOrigin own) children
    contextOrigin own (Transport _ parent) = contextOrigin own parent
    contextOrigin own (Super sel predicate parent) = validSuper sel predicate parent && contextOrigin own parent
    contextOrigin _ _ = False
    ordinary (Instance df predicate [] []) =
        not (runtimeOwns rt (getName df)) && case (boundInstance rt (getName df), getClassPredTys_maybe predicate) of
            (Just (cls, args), Just (actual, selected')) -> cls == getName actual && sameTypes args selected'
            _ -> False
    ordinary _ = False
    runtime active own method (Instance df predicate args [child])
        | Just p <- predicateOf child,
          Just (childMethod, childType) <- runtimeDemandMethod rt method df predicate args [p],
          Just (cls, _) <- getClassPredTys_maybe predicate =
            prove active own (Use childMethod (getName cls) ScalarMethod [childType] [child])
    runtime _ _ _ _ = False
    selected active own method evidence@(Instance df predicate args ds) =
        case [s | s@(Stock actual _ _ _ _) <- stocks, actual == df] of
            [Stock _ vars slots result methods] ->
                length vars == length args && length slots == length ds &&
                all (contextOrigin own) ds &&
                eqType (substTyWith vars args result) predicate &&
                and (zipWith (\v d -> maybe False (eqType (substTyWith vars args (idType v))) (predicateOf d)) slots ds) &&
                case [m | m@(StockMethod name def _ _ _ _ _) <- methods, method == name || def == Just method] of
                    [StockMethod name def implementation variables context obligations inherited]
                        | def == Just method -> inherited == Just method &&
                            case defaultDependency name of
                                Just dependency -> case getClassPredTys_maybe predicate of
                                    Just (cls, [operand]) -> prove active own (Use dependency (getName cls) ScalarMethod [operand] [evidence])
                                    _ -> False
                                _ -> False
                        | otherwise ->
                            any (\(m, previous) -> m == getName implementation && sameEvidence previous evidence) active ||
                            (all (progress evidence) active &&
                            let ty = substTyWith variables args
                                bindings = zip (map getName context) ds
                                apply (Use n c f ts es) = Use n c f (map ty ts) (map (substitute ty bindings) es)
                            in all (prove ((getName implementation, evidence) : active) own . apply) obligations)
                    _ -> False
            _ -> False
    selected _ _ _ _ = False
    defaultDependency name = lookup name [(selector, dependency) |
        (label, target) <- [("/=", "=="), ("show", "showsPrec"), ("showList", "showsPrec")],
        Just selector <- [lookup ("Prelude." ++ label) (standardNames rt)],
        Just dependency <- [lookup ("Prelude." ++ target) (standardNames rt)]]
    progress evidence@(Instance df _ args ds) (_, previous@(Instance ancestor _ oldArgs oldDs)) =
        df /= ancestor || descendant evidence previous ||
        (permutation eqType args oldArgs && permutation sameEvidence ds oldDs)
    progress _ _ = False
    permutation eq xs ys = length xs == length ys && null (deleteFirstsBy eq xs ys)
    sameTypes xs ys = length xs == length ys && and (zipWith eqType xs ys)
    resolve path (Transport target source) = do
        actual <- resolve path source
        pure $ case actual of
            Transport _ inner | maybe False (eqType target) (predicateOf inner) -> inner
            _ | maybe False (eqType target) (predicateOf actual) -> actual
              | otherwise -> Transport target actual
    resolve path (Instance df p ts ds) = Instance df p ts <$> mapM (resolve path) ds
    resolve path evidence@(Super sel predicate parent) = do
        guard (validSuper sel predicate parent)
        source <- resolve path parent
        case source of
            Given{} -> pure (Super sel predicate source)
            Transport{} -> pure (Super sel predicate source)
            Super{} -> pure (Super sel predicate source)
            Instance df p ts ds | getName df `notElem` path ->
                case Map.lookup (getName df) env of
                    Just rhs -> case C.collectArgs (snd (C.collectBinders rhs)) of
                        (C.Var worker, supplied) -> do
                            let (bs, body) = C.collectBinders rhs
                                (vs, slots) = span isTyVar bs
                                ty = substTyWith vs ts
                            guard (length vs == length ts && length slots == length ds &&
                                all (\v -> isId v && isPredTy (idType v)) slots && eqType (ty (exprType body)) p &&
                                and (zipWith (\v d -> maybe False (eqType (ty (idType v))) (predicateOf d)) slots ds))
                            (cls, _) <- getClassPredTys_maybe p
                            guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)))
                            let fields = filter (not . C.isTyCoArg) supplied
                            guard (length fields == length (classAllSelIds cls))
                            field <- lookup sel (zip (classAllSelIds cls) fields)
                            template <- fieldEvidence env [] (Set.fromList (map getName slots)) field
                            result <- resolve (getName df : path) (substitute ty (zip (map getName slots) ds) template)
                            actual <- predicateOf result
                            guard (eqType actual predicate)
                            pure result
                        _ -> projected homes evidence >>= resolve (getName df : path)
                    Nothing -> projected homes evidence >>= resolve (getName df : path)
            _ -> Nothing
    resolve _ e@Given{} = Just e
    resolve _ e@BuiltinTag{} = Just e
    resolve _ Unknown{} = Nothing
    -- Open only the actual selected superclass field. Helpers may abstract over
    -- types and evidence, never ordinary executable arguments. trace validates
    -- original coercions before any type/context substitution is performed.
    fieldEvidence local seen given (C.Let (C.NonRec binder rhs) body)
        | isId binder && isPredTy (idType binder) =
            fieldEvidence (Map.insert (getName binder) rhs local) seen given body
    fieldEvidence local seen given expression = case trace local given Set.empty expression of
        Unknown{} -> do
            (C.Var helper, supplied) <- pure (C.collectArgs expression)
            guard (getName helper `notElem` seen)
            rhs <- Map.lookup (getName helper) local
            let (bs, body) = C.collectBinders rhs
                (vs, slots) = span isTyVar bs
                ts = [t | C.Type t <- supplied]
                arguments = filter (not . C.isTyCoArg) supplied
            guard (length supplied == length vs + length slots && length ts == length vs &&
                length arguments == length slots && all (\v -> isId v && isPredTy (idType v)) slots &&
                all (isPredTy . exprType) arguments)
            values <- mapM (fieldEvidence local (getName helper : seen) given) arguments
            template <- fieldEvidence local (getName helper : seen) (Set.union given (Set.fromList (map getName slots))) body
            let ty = substTyWith vs ts
            guard (and (zipWith (\v d -> maybe False (eqType (ty (idType v))) (predicateOf d)) slots values))
            let result = substitute ty (zip (map getName slots) values) template
            guard (maybe False (eqType (exprType expression)) (predicateOf result))
            pure result
        evidence -> Just evidence
