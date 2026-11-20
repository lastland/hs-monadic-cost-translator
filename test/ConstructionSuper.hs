-- | Exact nominal superclass edges; validation grants no method permission.
module ConstructionSuper (validSuper) where

import ConstructionEvidenceTypes (Evidence, predicateOf)
import GHC
import GHC.Core.Class (classSCSelIds, classTyVars)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (substTyWith)

-- Substitute every type and kind argument of the actual parent class.
validSuper :: Id -> Type -> Evidence -> Bool
validSuper selector endpoint parent = case predicateOf parent >>= getClassPredTys_maybe of
    Just (cls, arguments)
        | length arguments == length (classTyVars cls)
        , Just predicate <- lookup selector (zip (classSCSelIds cls) (classSCTheta cls)) ->
            eqType endpoint (substTyWith (classTyVars cls) arguments predicate)
    _ -> False
