-- | Structural-view names and denial-only demand-type facts. Observation,
-- generation, derivation and constrained continuations confer no permissions.
module ConstructionRoles (supportCoreNames, homeDemandTypes) where

import ConstructionAst
import ConstructionRuntime
import Data.List (nub)
import Data.Maybe (mapMaybe)
import GHC
import GHC.Core.FamInstEnv (FamInst(..))
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Tc.Types (tcg_tcs, tcg_fam_insts)

-- These facts only reject executable Eq/Show/etc. uses on demand data. An
-- actual associated type equation is not authority to observe a thunk.
homeDemandTypes :: Runtime -> [TypecheckedModule] -> [TyCon]
homeDemandTypes rt checked = nub
    [tc | m <- checked, equation <- tcg_fam_insts (fst (tm_internals_ m)),
        fi_fam equation == runtimeName rt "Memorist.Data.Interface.Demand" "Dem",
        Just (tc, _) <- [splitTyConApp_maybe (fi_rhs equation)], tc `elem` homeTypes]
  where homeTypes = [tc | m <- checked, tc <- tcg_tcs (fst (tm_internals_ m))]

-- GHC gives instance methods fresh internal identifiers. Follow only the finite
-- AbsBinds export relation, not name spellings or IdInfo/unfoldings.
supportCoreNames :: [(SrcSpan, Name)] -> TypecheckedModule -> [Name]
supportCoreNames support checked = close initial
  where
    initial = [getName (unLoc name) | FunBind _ name _ <- collectAst (tm_typechecked_source checked) :: [HsBind GhcTc],
        getLocA name `elem` map fst support]
    exports = [(getName poly, getName mono) | ABE poly mono _ _ <- collectAst (tm_typechecked_source checked) :: [ABExport]]
    close names = let more = names ++ mapMaybe (\(poly, mono) -> if mono `elem` names && poly `notElem` names then Just poly else Nothing) exports
                  in if length more == length names then names else close more
