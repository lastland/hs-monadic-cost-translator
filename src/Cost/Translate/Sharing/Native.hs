module Cost.Translate.Sharing.Native where

import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Data.Map.Strict qualified as Map (Map, lookup)
import GHC
    ( Name
    , NamedThing (getName)
    , TyCon
    , TyVar
    , Type
    , nameSrcSpan
    )
import GHC.Builtin.Types
    ( liftedTypeKind
    , listTyCon
    , maybeTyCon
    , tupleTyCon
    )
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type
    ( getTyVar_maybe
    , mkTyVarTy
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    , substTyWith
    )
import GHC.Types.Basic (Boxity (..))
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
    [TyCon] ->
    Map.Map Name String ->
    Type ->
    Either String (Maybe (O.Ty, O.Expr))
nativeSharing datatypes renamings ty
    | null vars
        || not
            ( vacuousSharing ty
                || all ((`eqType` liftedTypeKind) . tyVarKind) vars
            )
        || not (null arguments) =
        Right Nothing
    | otherwise = case mapper vars result of
        Nothing -> Right Nothing
        Just convert -> do
            let (renamed, renamedBody) = sharingType renamings ty
            quantified <- mapM (renderBinder datatypes) renamed
            rendered <- renderType datatypes (snd (splitFunTys renamedBody))
            let action = O.tc "M.MemoristM" [rendered]
            pure (Just (O.forallT quantified action, convert))
    where
        (vars, body) = splitForAllTyVars ty
        (arguments, result) = splitFunTys body

vacuousSharing :: Type -> Bool
vacuousSharing ty =
    not (null variables)
        && not (any (`elemVarSet` tyCoVarsOfType body) variables)
    where
        (variables, body) = splitForAllTyVars ty

mapper :: [TyVar] -> Type -> Maybe O.Expr
mapper vars value
    | not (any (`elemVarSet` tyCoVarsOfType value) vars) = Just (O.v "id")
    | Just variable <- getTyVar_maybe value
    , variable `elem` vars =
        Just (O.v "Void.absurd")
    | Just (constructor, [element]) <- splitTyConApp_maybe value
    , constructor == listTyCon =
        O.call "mapListT" . (: []) <$> mapper vars element
    | Just (constructor, [element]) <- splitTyConApp_maybe value
    , constructor == maybeTyCon = do
        convert <- mapper vars element
        pure (maybeMapper convert)
    | Just (constructor, [left, right]) <- splitTyConApp_maybe value
    , constructor == tupleTyCon Boxed 2 = do
        convertLeft <- mapper vars left
        convertRight <- mapper vars right
        pure (pairMapper convertLeft convertRight)
    | otherwise = Nothing

maybeMapper :: O.Expr -> O.Expr
maybeMapper convert =
    O.lam
        [O.p "maybeValue"]
        ( O.caseE
            (O.v "maybeValue")
            [ (O.conP "M.NothingT" [], O.v "M.NothingT")
            ,
                ( O.conP "M.JustT" [O.p "payload"]
                , O.call "M.JustT" [mapThunk convert (O.v "payload")]
                )
            ]
        )

pairMapper :: O.Expr -> O.Expr -> O.Expr
pairMapper convertLeft convertRight =
    O.lam
        [O.p "pairValue"]
        ( O.caseE
            (O.v "pairValue")
            [
                ( O.conP "M.PairT" [O.p "leftPayload", O.p "rightPayload"]
                , O.call
                    "M.PairT"
                    [ mapThunk convertLeft (O.v "leftPayload")
                    , mapThunk convertRight (O.v "rightPayload")
                    ]
                )
            ]
        )

mapThunk :: O.Expr -> O.Expr -> O.Expr
mapThunk convert payload = O.call "mapThunk" [convert, payload]
