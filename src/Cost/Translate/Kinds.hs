module Cost.Translate.Kinds
    ( visibleArguments
    , visibleVariables
    , renderKind
    , renderBinder
    , kindSupport
    ) where

import Cost.Translate.Kinds.Lifting (lifting)
import Cost.Translate.Kinds.Mapping (mapping)
import Cost.Translate.Kinds.Requirements
    ( generatedRequirements
    , shapeRequirements
    )
import Cost.Translate.Names (generatedName, sourceConstructor)
import Cost.Translate.Output qualified as O
import GHC
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isVisibleTyConBinder, tyConBinders)
import GHC.Core.Type
    ( getTyVar_maybe
    , isLiftedTypeKind
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    )
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (getOccString)
import GHC.Types.Var (binderVar, tyVarKind)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

visibleArguments :: TyCon -> [a] -> [a]
visibleArguments tc args =
    [a | (binder, a) <- zip binders args, isVisibleTyConBinder binder]
        ++ drop (length binders) args
    where
        binders = tyConBinders tc

visibleVariables :: TyCon -> [TyVar]
visibleVariables = map binderVar . filter isVisibleTyConBinder . tyConBinders

renderKind :: [TyCon] -> Type -> Either String O.Ty
renderKind locals kind
    | isLiftedTypeKind kind = Right (O.t "Type")
    | Just v <- getTyVar_maybe kind = Right (O.t (getOccString v))
    | (args, result) <- splitFunTys kind
    , not (null args) = do
        inputs <- mapM (renderKind locals . scaledThing) args
        O.arrows inputs <$> renderKind locals result
    | Just (tc, args) <- splitTyConApp_maybe kind
    , tc `elem` locals = do
        pieces <- mapM (renderKind locals) (visibleArguments tc args)
        pure (O.tc (sourceConstructor (getName tc)) pieces)
    | Just (tc, args) <- splitTyConApp_maybe kind
    , Just name <- builtinKind tc = do
        pieces <- mapM (renderKind locals) (visibleArguments tc args)
        pure (O.tc name pieces)
    | otherwise = Left ("unsupported kind: " ++ showSDocUnsafe (ppr kind))
    where
        builtinKind tc
            | tc == listTyCon = Just "[]"
            | tc == maybeTyCon = Just "Maybe"
            | tc == tupleTyCon Boxed 2 = Just "(,)"
            | getOccString tc `elem` ["Bool", "Ordering", "Char"] =
                Just (getOccString tc)
            | getOccString tc `elem` ["Nat", "Natural"] = Just "TypeLits.Nat"
            | getOccString tc == "Symbol" = Just "TypeLits.Symbol"
            | otherwise = Nothing

renderBinder :: [TyCon] -> TyVar -> Either String O.Binder
renderBinder locals v = do
    kind <- renderKind locals (tyVarKind v)
    pure (O.bk (getOccString v) kind)

kindSupport :: [TyCon] -> Either String [O.Decl]
kindSupport [] = Right []
kindSupport types = do
    cases <-
        mapM
            mappingCase
            [tc | tc <- types, not (isLiftedTypeKind (tyConKind tc))]
    pure $
        mapping
            "Rep"
            "M.Rep"
            repBuiltins
            [(kind, s, t) | (kind, s, t, _) <- cases]
            ++ mapping
                "Dem"
                "M.Dem"
                demBuiltins
                [(kind, t, d) | (kind, _, t, d) <- cases]
            ++ lifting "Repr" "RepOf" "M.Repr"
            ++ lifting "Demand" "DemOf" "M.Demand"
            ++ generatedRequirements
            ++ shapeRequirements
            ++ constraintSupport
    where
        typ = O.t "Type"
        mappingCase tc = do
            kind <- renderKind types (snd (splitForAllTyVars (tyConKind tc)))
            pure
                ( kind
                , sourceConstructor (getName tc)
                , generatedName "T" (getName tc)
                , generatedName "D" (getName tc)
                )
        repBuiltins =
            [ (O.arrow typ typ, "[]", "L.ListT")
            , (O.arrow typ typ, "Maybe", "M.MaybeT")
            , (O.arrows [typ, typ] typ, "(,)", "M.PairT")
            ]
        demBuiltins =
            [ (O.arrow typ typ, "L.ListT", "L.ListD")
            , (O.arrow typ typ, "M.MaybeT", "M.MaybeD")
            , (O.arrows [typ, typ] typ, "M.PairT", "M.PairD")
            ]

constraintSupport :: [O.Decl]
constraintSupport =
    [ O.sig
        "withConstraint"
        (O.qualified [c] (O.arrows [O.tc "Proxy" [c], O.qualified [c] r] r))
    , O.val "withConstraint" [O.p "_", O.p "body"] (O.v "body")
    ]
    where
        c = O.t "c"
        r = O.t "r"
