-- | A closed home dictionary can select a default that only builds data.
-- Keep the retained class equation and the actual DFun field as separate facts:
-- the class and its instance need not be defined in the same home module.
module ConstructionDefaults (Defaults, homeDefaults, selectsDataDefault) where

import ConstructionAst (collectAst, equations)
import ConstructionDictionary (Environment, Evidence(..), Use(..), trace)
import ConstructionRuntime (Family(UnknownMethod))
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classOpItems, classTyCon)
import GHC.Core.Coercion (Role(Nominal))
import GHC.Core.DataCon (dataConRepArity)
import GHC.Core.FamInstEnv (FamInstEnvs, reduceTyFamApp_maybe)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isDataTyCon, tyConSingleDataCon)
import GHC.Core.Type (isPredTy, splitFunTys, splitTyConApp_maybe)
import GHC.Tc.Types (tcg_tcs)
import GHC.Types.Basic (DefMethSpec(VanillaDM))
import GHC.Types.Id (isDFunId, isDataConId_maybe, isDataConWorkId_maybe)
import GHC.Types.Name (isDataConName)

data DefaultField = DefaultField { fieldDFun :: Name, fieldSelector :: Name, fieldDefault :: Name }
data Defaults = Defaults [Name] [DefaultField]

homeDefaults :: TypecheckedModule -> C.CoreProgram -> FamInstEnvs -> Environment -> [(Id, C.CoreExpr)] -> Defaults
homeDefaults checked program families env raw = Defaults bodies (concatMap selected raw)
  where
    source = [unLoc n | Just (group, _, _, _, _) <- [tm_renamed_source checked],
        ClassDecl{tcdMeths = methods} <- collectAst group :: [TyClDecl GhcRn],
        L _ b@(FunBind _ n _) <- toList methods, Just [(pats, rhs)] <- [equations b],
        all ignored pats, dataSyntax rhs]
    bodies = [dm | tc <- tcg_tcs (fst (tm_internals_ checked)), Just cls <- [tyConClass_maybe tc],
        (sel, Just (dm, VanillaDM)) <- classOpItems cls, getName sel `elem` source,
        (v, rhs) <- C.flattenBinds program, getName v == dm, dataTree (snd (C.collectBinders rhs))]
    -- Match the unsimplified dictionary constructor, including single-method
    -- classes before newtype packing turns their dictionary into a cast.
    selected (df, dictionary) = do
        guard (isDFunId df)
        Just (cls, [ty]) <- pure (getClassPredTys_maybe (idType df))
        (C.Var con, C.Type arg : fields) <- pure (C.collectArgs dictionary)
        guard (isDataConId_maybe con == Just (tyConSingleDataCon (classTyCon cls)) && eqType arg ty)
        let selectors = classAllSelIds cls
        guard (length selectors == length fields)
        (sel, Just (dm, VanillaDM)) <- classOpItems cls
        Just (C.Var method) <- pure (lookup sel (zip selectors fields))
        Just rhs <- pure (Map.lookup (getName method) env)
        (C.Var actual, [C.Type t, d]) <- pure (C.collectArgs (evidenceBody rhs))
        guard (getName actual == dm && eqType t ty)
        Instance owner predicate [] [] <- pure (trace env Set.empty Set.empty d)
        guard (owner == df && eqType predicate (idType df) && firstOrderMethod families (idType method))
        pure DefaultField { fieldDFun = getName df, fieldSelector = getName sel, fieldDefault = dm }
    evidenceBody (C.Let (C.NonRec v _) body) | isPredTy (idType v) = evidenceBody body
    evidenceBody body = body

-- Inspect the whole retained equation: dead alternatives, guards or local
-- helpers can disappear even before the desugared default looks constant.
ignored :: LPat GhcRn -> Bool
ignored (L _ WildPat{}) = True
ignored (L _ VarPat{}) = True
ignored _ = False

dataSyntax :: LHsExpr GhcRn -> Bool
dataSyntax (L _ expression) = case expression of
    HsVar _ n -> isDataConName (unLoc n)
    HsApp _ f x -> dataSyntax f && dataSyntax x
    HsPar _ x -> dataSyntax x
    ExplicitList _ xs -> all dataSyntax xs
    HsLit{} -> True
    _ -> False

-- Check the elaboration too. Neither a constructor wrapper with an executable
-- dependency nor a partially applied constructor is a closed tree of data.
dataTree :: C.CoreExpr -> Bool
dataTree C.Lit{} = True
dataTree e = case C.collectArgs e of
    (C.Var v, args) | Just dc <- isDataConWorkId_maybe v, let fields = filter (not . C.isTypeArg) args ->
        length fields == dataConRepArity dc && all dataTree fields
    _ -> False

-- Require first-order data at the selected field, not just at the class
-- signature. One family reduction per path suffices for scalar demand types.
-- A newtype has no runtime constructor and can represent a function outright.
firstOrderMethod :: FamInstEnvs -> Type -> Bool
firstOrderMethod families signature = all (dataTy True) (result : map scaledThing args)
  where
    (args, result) = splitFunTys signature
    dataTy canReduceFamily ty = case splitTyConApp_maybe ty of
        Just (tc, ts)
            | isFamilyTyCon tc -> canReduceFamily && maybe False (dataTy False . reductionReducedType)
                    (reduceTyFamApp_maybe families Nominal tc ts)
            | isDataTyCon tc && not (isPredTy ty) -> all (dataTy canReduceFamily) ts
        _ -> False

selectsDataDefault :: [Defaults] -> Use -> Bool
selectsDataDefault modules = selected
  where
    bodies = Set.fromList [dm | Defaults ds _ <- modules, dm <- ds]
    methods = Set.fromList [(fieldDFun field, n) | Defaults _ fields <- modules, field <- fields,
        fieldDefault field `Set.member` bodies, n <- [fieldSelector field, fieldDefault field]]
    selected (Use n _ UnknownMethod [ty] [Instance df predicate [] []])
        | (getName df, n) `Set.member` methods, eqType predicate (idType df),
          Just (_, [t]) <- getClassPredTys_maybe predicate = eqType t ty
    selected _ = False
