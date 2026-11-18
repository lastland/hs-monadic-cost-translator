module Cost.Translate.Datatypes.Types (renderType, render) where

import Cost.Translate.Kinds (visibleArguments)
import Cost.Translate.Names (generatedName, sourceConstructor)
import Cost.Translate.Output qualified as O
import GHC
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon, unitTyCon)
import GHC.Core.TyCon (isPromotedDataCon)
import GHC.Core.Type
    ( getTyVar_maybe
    , isCharLitTy
    , isNumLitTy
    , isStrLitTy
    , splitAppTy_maybe
    , splitTyConApp_maybe
    )
import GHC.Data.FastString (unpackFS)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

renderType :: [TyCon] -> Type -> Either String O.Ty
renderType = render "T"

render :: String -> [TyCon] -> Type -> Either String O.Ty
render suffix locals ty
    | Just v <- getTyVar_maybe ty = Right (O.t (getOccString v))
    | Just n <- isNumLitTy ty = Right (O.numT n)
    | Just s <- isStrLitTy ty = Right (O.strT (unpackFS s))
    | Just c <- isCharLitTy ty = Right (O.charT c)
    | Just (tc, args) <- splitTyConApp_maybe ty = do
        rendered <- mapM recurse (visibleArguments tc args)
        renderConstructor suffix locals ty tc args rendered
    | Just (f, a) <- splitAppTy_maybe ty =
        O.tapp <$> recurse f <*> ((: []) <$> recurse a)
    | otherwise = unsupportedType ty
    where
        recurse = render suffix locals

renderConstructor ::
    String -> [TyCon] -> Type -> TyCon -> [Type] -> [O.Ty] -> Either String O.Ty
renderConstructor suffix locals ty tc args rendered
    | isPromotedDataCon tc = pure (O.promoted (promotedName locals tc) rendered)
    | tc == unitTyCon = pure (O.t "()")
    | tc == listTyCon = pure (O.tc (builtin "[]" "L.List") rendered)
    | tc == maybeTyCon = pure (O.tc (builtin "Maybe" "M.Maybe") rendered)
    | tc == tupleTyCon Boxed 2 = pure (O.tc (builtin "(,)" "M.Pair") rendered)
    | tc `elem` locals = pure (O.tc localName rendered)
    | null args
    , getOccString tc `elem` primitiveTypes =
        pure (O.t (getOccString tc))
    | otherwise = unsupportedType ty
    where
        builtin original transformed =
            if null suffix then original else transformed ++ suffix
        localName =
            if null suffix
                then sourceConstructor (getName tc)
                else generatedName suffix (getName tc)

promotedName :: [TyCon] -> TyCon -> String
promotedName locals tc = case getOccString tc of
    original@"[]" -> original
    original@":" -> original
    tuple@('(' : _) -> tuple
    identifier -> qualifier ++ identifier
    where
        qualifier =
            if any sameModule locals then "Source." else "Prelude."
        sameModule local =
            nameModule_maybe (getName local) == nameModule_maybe (getName tc)

primitiveTypes :: [String]
primitiveTypes = ["Int", "Integer", "Bool", "Float", "Double", "Char"]

unsupportedType :: Type -> Either String a
unsupportedType ty =
    Left ("unsupported type representation: " ++ showSDocUnsafe (ppr ty))
