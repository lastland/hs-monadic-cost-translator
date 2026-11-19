-- | Identify declaration roles in the typechecked home graph.
module ConstructionRoles
    ( QuickCheck, quickCheckRoles, isQuickCheckBinding
    , supportCoreNames, homeDemandTypes
    ) where

import ConstructionAst
import ConstructionRuntime
import Data.List (nub)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.FamInstEnv (FamInst(..))
import GHC.Core.Type (splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (fsLit)
import GHC.Tc.Types (tcg_tcs, tcg_fam_insts)

-- Only the result after the binders counts: Gen in an argument, or inside
-- MemoristM, does not turn a translated computation into a generator.
newtype QuickCheck = QuickCheck (Set.Set Name)

isQuickCheckBinding :: NamedThing a => QuickCheck -> a -> Bool
isQuickCheckBinding (QuickCheck names) = (`Set.member` names) . getName

quickCheckRoles :: [TypecheckedModule] -> Ghc QuickCheck
quickCheckRoles modules = do
    qc <- findModule (mkModuleName "Test.QuickCheck") (Just (fsLit "QuickCheck"))
    constructors <- mapM (resolveType qc) ["Gen", "Property"]
    let declared = Set.fromList [name | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group, Just name <- [bindingName binding]]
        -- A renamed method refers to its class selector. The typechecked binds
        -- and their exports identify its implementations, and also name local
        -- generators. Do not grant a role to an arbitrary intermediate Core
        -- expression just because its type happens to be Gen.
        locals m = [unLoc n | FunBind _ n _ <- collectAst (tm_typechecked_source m) :: [HsBind GhcTc]] ++
            [v | ABE poly mono _ _ <- collectAst (tm_typechecked_source m), v <- [poly, mono]]
    things <- mapM lookupName (filter isExternalName (Set.toList declared))
    let ids = [v | Just (AnId v) <- things] ++ concatMap locals modules
    pure (QuickCheck (Set.fromList [getName v | v <- ids,
        Just (tc, _) <- [splitTyConApp_maybe (snd (splitPiTys (idType v)))], tc `elem` constructors]))

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
