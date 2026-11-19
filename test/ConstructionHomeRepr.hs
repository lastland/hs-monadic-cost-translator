-- | Link a closed home Repr dictionary to its actual reprM implementation.
-- These are conditional certificates: every module body and retained source
-- site is checked, and every caller must supply the audited carrier.
module ConstructionHomeRepr (HomeReprs, homeReprs, selectedHome, methodCarrier) where

import ConstructionAst (collectAst)
import ConstructionRuntime
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (noFreeVarsOfType)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Id (isDFunId)

data HomeReprs = HomeReprs (Map.Map Name Type) (Set.Set Name)

homeReprs :: Runtime -> [(TypecheckedModule, [C.CoreBind])] -> HomeReprs
homeReprs rt programs = HomeReprs (Map.fromList [(owner, ty) | (owner, ty, _) <- entries])
    (Set.fromList [carrier | (_, _, carrier) <- entries])
  where
    entries = [(getName owner, ty, getName carrier) | (checked, program) <- programs,
        let bindings = C.flattenBinds program,
        let sources = sourceFields rt checked,
        (owner, dictionary) <- bindings, isDFunId owner,
        Just (cls, [ty]) <- [getClassPredTys_maybe (idType owner)], noFreeVarsOfType ty,
        getName cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr",
        (C.Var constructor, C.Type headTy : fields) <- [C.collectArgs dictionary], eqType ty headTy,
        constructor == dataConWorkId (tyConSingleDataCon (classTyCon cls)),
        let selectors = classAllSelIds cls, length selectors == length fields,
        (selector, C.Var implementation) <- zip selectors fields, roleOf rt (getName selector) == Just ReprMethod,
        (method, carrier) <- sources, implementation == method,
        Just (carrierClass, [_, _]) <- [getClassPredTys_maybe (idType carrier)],
        getName carrierClass == runtimeName rt "Memorist.Base.Core" "Memorist"]

-- Link the field back to its retained source binder as well. Inlining or
-- aliases must never turn an ordinary generic helper into a method body.
sourceFields :: Runtime -> TypecheckedModule -> [(Id, Id)]
sourceFields rt checked = [(poly, carrier) | ab <- collectAst (tm_typechecked_source checked),
    ABE poly _ _ _ <- abs_exports ab,
    FunBind (inner, _) name _ <- collectAst (abs_binds ab) :: [HsBind GhcTc], getLocA name `elem` locations,
    carrier <- wrapperVars inner]
  where
    wrapperVars (WpCompose a b) = wrapperVars a ++ wrapperVars b
    wrapperVars (WpEvLam v) = [v]
    wrapperVars _ = []
    locations = [getLocA name | Just (group, _, _, _, _) <- [tm_renamed_source checked],
        inst <- collectAst group :: [ClsInstDecl GhcRn], L _ (FunBind _ name _) <- toList (cid_binds inst),
        roleOf rt (unLoc name) == Just ReprMethod]

selectedHome :: HomeReprs -> Name -> Type -> Bool
selectedHome (HomeReprs heads _) owner ty = maybe False (eqType ty) (Map.lookup owner heads)

methodCarrier :: HomeReprs -> Name -> Bool
methodCarrier (HomeReprs _ carriers) = (`Set.member` carriers)
