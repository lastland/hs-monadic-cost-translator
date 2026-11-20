-- | Finite proof syntax. Evidence origins retain exact Names, not printed types.
module ConstructionEvidenceTypes (Evidence(..), Argument(..), predicateOf) where

import GHC
import GHC.Core.Coercion (Coercion, coercionRKind)

data Evidence = Given Name Type | Instance Id Type [Type] [Evidence] | Super Id Type Evidence
    | Transport Type Evidence | BuiltinTag Type Type Id | Unknown String
    | TypeLambda Type TyVar Evidence | EvidenceLambda Type Id Evidence
    | Application Type Evidence Argument | Lifted Coercion Evidence
    | Equality Type Coercion
    | EqualityCase Type Evidence Evidence
data Argument = TypeArgument Type | EvidenceArgument Evidence

predicateOf :: Evidence -> Maybe Type
predicateOf (Super _ predicate _) = Just predicate
predicateOf (Transport predicate _) = Just predicate
predicateOf (Given _ predicate) = Just predicate
predicateOf (Instance _ predicate _ _) = Just predicate
predicateOf (BuiltinTag predicate _ _) = Just predicate
predicateOf (TypeLambda predicate _ _) = Just predicate
predicateOf (EvidenceLambda predicate _ _) = Just predicate
predicateOf (Application predicate _ _) = Just predicate
predicateOf (Lifted co _) = Just (coercionRKind co)
predicateOf (Equality predicate _) = Just predicate
predicateOf (EqualityCase predicate _ _) = Just predicate
predicateOf Unknown{} = Nothing
