-- | Conditional declaration proofs and exact concrete stock-field replay.
module ConstructionStock (stockUse) where

import ConstructionDemandMethods
import ConstructionDictionary
import ConstructionHomeRepr (HomeReprs)
import ConstructionMethods (MethodSite(..))
import ConstructionRuntime hiding (runtime)
import Data.List (deleteFirstsBy)
import GHC
import GHC.Core.Class (classSCSelIds, classTyVars)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (substTyWith)

stockUse :: Runtime -> HomeReprs -> [StockMethods] -> MethodSite -> (Use -> Bool) -> Use -> Bool
stockUse rt homes programs origin tag use =
    case [slots | Stock _ _ _ _ ms <- stocks, StockMethod _ _ impl _ slots _ _ <- ms,
            MethodBinding owner <- [origin], owner == getName impl] of
        [slots] -> prove [] slots use
        [] | homeEntry use -> prove [] [] use
        _ -> False
  where
    stocks = concat [ss | StockMethods _ ss <- programs]
    homeEntry (Use _ _ ScalarMethod _ [e]) = case resolve e of
        Just (Instance df _ _ _) -> runtimeOwns rt (getName df) || any (\(Stock owner _ _ _ _) -> df == owner) stocks
        _ -> False
    homeEntry _ = False
    prove _ _ obligation | tag obligation = True
    prove active own (Use method expected ScalarMethod [operand] [e])
        | not (isProtected rt operand), Just ev <- resolve e,
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
        | Just (childMethod, childType) <- runtimeDemandMethod rt method df predicate args [p],
          Just (cls, _) <- getClassPredTys_maybe predicate =
            prove active own (Use childMethod (getName cls) ScalarMethod [childType] [child])
      where p = case predicateOf child of
                Just actual -> actual
                Nothing -> predicate
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
    validSuper sel predicate parent = case predicateOf parent >>= getClassPredTys_maybe of
        Just (cls, args) -> case lookup sel (zip (classSCSelIds cls) (classSCTheta cls)) of
            Just formal -> eqType predicate (substTyWith (classTyVars cls) args formal)
            _ -> False
        _ -> False
    resolve = projected homes
