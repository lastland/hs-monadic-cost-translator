-- | Caller arithmetic has the source contract, not a preselected numeric
-- instance. This does not prove strictness from Num or excuse visible bodies.
module ConstructionArithmetic (callerArithmetic, wellFormed) where

import ConstructionDictionary (Evidence(..), Use(..), predicateOf)
import ConstructionRuntime (Runtime, Family(..), standardNames)
import GHC
import GHC.Builtin.Names (numClassName, fractionalClassName, integralClassName)
import GHC.Core.Class (classSCSelIds, classTyVars)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Types.Id (isDFunId)

callerArithmetic :: Runtime -> Use -> Bool
callerArithmetic rt (Use selected cls ScalarMethod [ty] [evidence]) =
    any matches operations && selectedPredicate && wellFormed evidence
  where
    -- Exactly the translator's strict arithmetic primitives, negation and
    -- overloaded literals. Eq/Ord/Show and other numeric methods are not covered.
    operations =
        [(op, numClassName) | op <- ["+", "-", "*", "negate", "fromInteger"]] ++
        [(op, fractionalClassName) | op <- ["/", "fromRational"]] ++
        [(op, integralClassName) | op <- ["div", "mod", "quot", "rem"]]
    matches (op, owner) = cls == owner && lookup ("Prelude." ++ op) (standardNames rt) == Just selected
    selectedPredicate = case predicateOf evidence >>= getClassPredTys_maybe of
        Just (actual, [operand]) -> getName actual == cls && eqType operand ty
        _ -> False
callerArithmetic _ _ = False

-- Check every selected superclass edge, including predicates after type
-- substitution. A matching printed class/type is not dictionary provenance.
wellFormed :: Evidence -> Bool
wellFormed (Given _ predicate) = case getClassPredTys_maybe predicate of
    Just _ -> True
    _ -> False
wellFormed (Super selector predicate parent) = case predicateOf parent >>= getClassPredTys_maybe of
    Just (cls, args) -> wellFormed parent && any
        (\(sel, super) -> sel == selector && eqType predicate (substTyWith (classTyVars cls) args super))
        (zip (classSCSelIds cls) (classSCTheta cls))
    _ -> False
wellFormed (Instance df predicate args children) =
    isDFunId df && length variables == length args && eqType predicate (instantiate result) &&
        length context == length children && and (zipWith matches context children)
  where
    (variables, body) = splitForAllTyVars (idType df)
    (context, result) = splitFunTys body
    instantiate = substTyWith variables args
    matches formal child = wellFormed child && maybe False (eqType (instantiate (scaledThing formal))) (predicateOf child)
wellFormed _ = False
