module Cost.Translate.Sharing.Native where

import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing.Schema
import Data.Map.Strict qualified as Map (Map, lookup)
import GHC
    ( Name
    , NamedThing (getName)
    , TyVar
    , Type
    , nameSrcSpan
    )
import GHC.Builtin.Types (liftedTypeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type
    ( mkTyVarTy
    , splitForAllTyVars
    , splitFunTys
    , substTyWith
    )
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (getUnique)
import GHC.Types.Var (setVarName, tyVarKind)
import GHC.Types.Var.Set (elemVarSet)

sharingType :: Map.Map Name String -> Type -> ([TyVar], Type)
sharingType renamings ty = (renamed, renamedBody)
    where
        (vars, body) = splitForAllTyVars ty
        renamed = map rename vars
        renamedBody = substTyWith vars (map mkTyVarTy renamed) body
        rename variable = case Map.lookup (getName variable) renamings of
            Nothing -> variable
            Just occurrence ->
                setVarName
                    variable
                    ( mkInternalName
                        (getUnique variable)
                        (mkTyVarOcc occurrence)
                        (nameSrcSpan (getName variable))
                    )

nativeSharing ::
    NativeSchemas ->
    Map.Map Name String ->
    Type ->
    Either String (Maybe (O.Ty, O.Expr))
nativeSharing schemas renamings ty
    | null vars
        || not
            ( vacuousSharing ty
                || all ((`eqType` liftedTypeKind) . tyVarKind) vars
            )
        || not (null arguments) =
        Right Nothing
    | otherwise = case schemaMapper schemas vars result of
        Nothing -> Right Nothing
        Just convert -> do
            let (renamed, renamedBody) = sharingType renamings ty
            quantified <- mapM (renderBinder datatypes) renamed
            rendered <- renderType datatypes (snd (splitFunTys renamedBody))
            let action = O.tc "M.MemoristM" [rendered]
            pure (Just (O.forallT quantified action, convert))
    where
        datatypes = schemaDatatypes schemas
        (vars, body) = splitForAllTyVars ty
        (arguments, result) = splitFunTys body

vacuousSharing :: Type -> Bool
vacuousSharing ty =
    not (null variables)
        && not (any (`elemVarSet` tyCoVarsOfType body) variables)
    where
        (variables, body) = splitForAllTyVars ty
