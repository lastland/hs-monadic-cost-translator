-- | Kind-indexed representation/demand mappings and their constraint
-- liftings, emitted as the output's kind-support section.
--
-- @RepOf@ and @DemOf@ are closed type families indexed by kind. At kind
-- @Type@ they reuse the runtime @Rep@ / @Dem@ families; at constructor
-- kinds they map lists, Maybe, pairs and the module's own datatypes —
-- including partial applications — to their generated counterparts, with a
-- recursion equation for applications.
--
-- The lifting layer packages the runtime classes as the @ReprK@ /
-- @DemandK@ constraint synonyms plus the @ReprApp@ / @DemandApp@ witness
-- classes, which carry the mapping equalities. Explicit witnesses are
-- needed for recursive and nested applications, where GHC cannot infer an
-- application dictionary from an equality wanted alone.
--
-- @GeneratedReq@ / @GeneratedK@ / @GeneratedF@ lift the bounded structural
-- generator interface across kinds; @ShapeReq@ / @ShapeK@ / @ShapeF@ do
-- the same for Eq/Show/Arbitrary dictionaries over demand values;
-- @withConstraint@ discharges a quantified constraint trivially.
--
-- Standalone kind signatures are essential throughout: each mapping's
-- result kind depends on its explicit kind argument, so the definitions
-- are polymorphically recursive (see the inline note on 'kindSupport').
module Cost.Translate.Kinds
    ( visibleArguments, visibleVariables, renderKind, renderBinder, kindSupport
    ) where

import GHC
import GHC.Core.Type (getTyVar_maybe, isLiftedTypeKind, splitForAllTyVars, splitFunTys, splitTyConApp_maybe)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isVisibleTyConBinder, tyConBinders)
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import GHC.Types.Basic (Boxity(..))
import GHC.Types.Var (binderVar, tyVarKind)
import GHC.Types.Name (getOccString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Cost.Translate.Names (generatedName, sourceConstructor)
import qualified Cost.Translate.Output as O

visibleArguments :: TyCon -> [a] -> [a]
visibleArguments tc args =
    [a | (binder,a) <- zip binders args, isVisibleTyConBinder binder] ++ drop (length binders) args
  where binders = tyConBinders tc

visibleVariables :: TyCon -> [TyVar]
visibleVariables = map binderVar . filter isVisibleTyConBinder . tyConBinders

renderKind :: [TyCon] -> Type -> Either String O.Ty
renderKind locals kind
    | isLiftedTypeKind kind = Right (O.t "Type")
    | Just v <- getTyVar_maybe kind = Right (O.t (getOccString v))
    | (args,result) <- splitFunTys kind, not (null args) = do
        inputs <- mapM (renderKind locals . scaledThing) args
        O.arrows inputs <$> renderKind locals result
    | Just (tc,args) <- splitTyConApp_maybe kind, tc `elem` locals = do
        pieces <- mapM (renderKind locals) (visibleArguments tc args)
        pure (O.tc (sourceConstructor (getName tc)) pieces)
    | Just (tc,args) <- splitTyConApp_maybe kind, Just name <- builtinKind tc = do
        pieces <- mapM (renderKind locals) (visibleArguments tc args)
        pure (O.tc name pieces)
    | otherwise = Left ("unsupported kind: " ++ showSDocUnsafe (ppr kind))
  where
    builtinKind tc
        | tc == listTyCon = Just "[]"
        | tc == maybeTyCon = Just "Maybe"
        | tc == tupleTyCon Boxed 2 = Just "(,)"
        | getOccString tc `elem` ["Bool","Ordering","Char"] = Just (getOccString tc)
        | getOccString tc `elem` ["Nat","Natural"] = Just "TypeLits.Nat"
        | getOccString tc == "Symbol" = Just "TypeLits.Symbol"
        | otherwise = Nothing

renderBinder :: [TyCon] -> TyVar -> Either String O.Binder
renderBinder locals v = do
    kind <- renderKind locals (tyVarKind v)
    pure (O.bk (getOccString v) kind)

-- These are generated dictionary adapters, not runtime replacements. At Type
-- they use the external classes; at an arrow kind they lift the same obligation
-- through application. At index kinds they carry no value-level obligation.
-- Standalone kind signatures are essential: the mapping's result kind depends
-- on its explicit first argument, and must remain polymorphically recursive.
kindSupport :: [TyCon] -> Either String [O.Decl]
kindSupport [] = Right []
kindSupport types = do
    cases <- mapM mappingCase [tc | tc <- types, not (isLiftedTypeKind (tyConKind tc))]
    pure $ mapping "Rep" "M.Rep" repBuiltins [(kind,s,t) | (kind,s,t,_) <- cases]
        ++ mapping "Dem" "M.Dem" demBuiltins [(kind,t,d) | (kind,_,t,d) <- cases]
        ++ lifting "Repr" "RepOf" "M.Repr"
        ++ lifting "Demand" "DemOf" "M.Demand"
        ++ [ O.kindSig "GeneratedReq" (O.forallVisible [O.b "k"] (O.arrow k constraint))
           , O.closedFamily "GeneratedReq" (map O.b ["k","a"])
                [([typ,a], O.tc "GeneratedDemand" [a]), ([O.arrow k l,f], O.tc "GeneratedF" [f]), ([k,a], O.t "()")]
           , O.kindSig "GeneratedK" (O.forallT [O.b "k"] (O.arrow k constraint))
           , O.classD generatedContext "GeneratedK" [O.bk "a" k] []
           , O.instanceD (O.qualified generatedContext (O.tc "GeneratedK" [O.kinded a k])) []
           , O.kindSig "GeneratedF" (O.forallT (map O.b ["k","l"]) (O.arrow (O.arrow k l) constraint))
           , O.classD [generatedLift] "GeneratedF" [O.b "f"] []
           , O.instanceD (O.qualified [generatedLift] (O.tc "GeneratedF" [f])) []
           , O.kindSig "ShapeReq" (O.arrow classKind (O.forallVisible [O.b "k"] (O.arrow k constraint)))
           , O.closedFamily "ShapeReq" (map O.b ["c","k","a"])
                [([c,typ,a], O.tapp c [a]), ([c,O.arrow k l,f], O.tc "ShapeF" [c,f]), ([c,k,a], O.t "()")]
           , O.kindSig "ShapeK" (O.arrow classKind (O.forallT [O.b "k"] (O.arrow k constraint)))
           , O.classD [O.tc "ShapeReq" [c,k,a]] "ShapeK" [O.b "c",O.bk "a" k] []
           , O.instanceD (O.qualified [O.tc "ShapeReq" [c,k,a]] (O.tc "ShapeK" [c,O.kinded a k])) []
           , O.kindSig "ShapeF" (O.arrow classKind (O.forallT (map O.b ["k","l"]) (O.arrow (O.arrow k l) constraint)))
           , O.classD [shapeLift] "ShapeF" (map O.b ["c","f"]) []
           , O.instanceD (O.qualified [shapeLift] (O.tc "ShapeF" [c,f])) []
           , O.sig "withConstraint" (O.qualified [c] (O.arrows [O.tc "Proxy" [c],O.qualified [c] r] r))
           , O.val "withConstraint" [O.p "_",O.p "body"] (O.v "body")
           ]
  where
    typ = O.t "Type"; constraint = O.t "Constraint"
    a = O.t "a"; c = O.t "c"; f = O.t "f"; k = O.t "k"; l = O.t "l"; r = O.t "r"
    classKind = O.arrow typ constraint
    generatedContext = [O.tc "DemandK" [a], O.tc "GeneratedReq" [k,a]]
    generatedLift = O.forallT [O.b "a"] (O.qualified [O.tc "GeneratedK" [a]] (O.tc "GeneratedK" [O.tapp f [a]]))
    shapeLift = O.forallT [O.b "a"] (O.qualified [O.tc "ShapeK" [c,a]] (O.tc "ShapeK" [c,O.tapp f [a]]))
    mappingCase tc = do
        kind <- renderKind types (snd (splitForAllTyVars (tyConKind tc)))
        pure (kind, sourceConstructor (getName tc), generatedName "T" (getName tc), generatedName "D" (getName tc))
    repBuiltins = [(O.arrow typ typ,"[]","L.ListT"), (O.arrow typ typ,"Maybe","M.MaybeT"), (O.arrows [typ,typ] typ,"(,)","M.PairT")]
    demBuiltins = [(O.arrow typ typ,"L.ListT","L.ListD"), (O.arrow typ typ,"M.MaybeT","M.MaybeD"), (O.arrows [typ,typ] typ,"M.PairT","M.PairD")]

mapping :: String -> String -> [(O.Ty,String,String)] -> [(O.Ty,String,String)] -> [O.Decl]
mapping name base builtins locals =
    [ O.kindSig (name ++ "Of") (O.forallT [O.b "k"] (O.arrow k k))
    , O.typeSyn (name ++ "Of") [O.bk "a" k] (O.tc (name ++ "K") [k,a])
    , O.kindSig (name ++ "K") (O.forallVisible [O.b "k"] (O.arrow k k))
    , O.closedFamily (name ++ "K") (map O.b ["k","a"])
        (([O.t "Type",a],O.tc base [a]) :
         [([kind,O.t source], O.t target) | (kind,source,target) <- builtins ++ locals] ++
         [([k,O.tapp f [O.kinded a j]], O.tc (name ++ "K") [O.arrow j k,f,O.tc (name ++ "K") [j,a]]), ([k,a],a)])
    ]
  where a = O.t "a"; f = O.t "f"; j = O.t "j"; k = O.t "k"

lifting :: String -> String -> String -> [O.Decl]
lifting name family base =
    [ O.kindSig (name ++ "Req") (O.forallVisible [O.b "k"] (O.arrow k constraint))
    , O.closedFamily (name ++ "Req") (map O.b ["k","a"])
        [([O.t "Type",a],O.tc base [a]), ([O.arrow k l,f],O.tc (name ++ "F") [f]), ([k,a],O.t "()")]
    , O.kindSig (name ++ "K") (O.forallT [O.b "k"] (O.arrow k constraint))
    , O.typeSyn (name ++ "K") [O.bk "a" k] (O.tc (name ++ "Req") [k,a])
    , O.kindSig (name ++ "App") (O.forallT (map O.b ["k","l"]) (O.arrows [O.arrow k l,k] constraint))
    , O.classD predicates (name ++ "App") (map O.b ["f","a"]) []
    , O.instanceD (O.qualified predicates (O.tc (name ++ "App") [f,a])) []
    , O.kindSig (name ++ "F") (O.forallT (map O.b ["k","l"]) (O.arrow (O.arrow k l) constraint))
    , O.classD [lifted] (name ++ "F") [O.b "f"] []
    , O.instanceD (O.qualified [lifted] (O.tc (name ++ "F") [f])) []
    ]
  where
    a = O.t "a"; f = O.t "f"; k = O.t "k"; l = O.t "l"; constraint = O.t "Constraint"
    predicates = [O.tc (name ++ "K") [O.tapp f [a]], O.equalT (O.tc family [O.tapp f [a]]) (O.tc family [f,O.tc family [a]])]
    lifted = O.forallT [O.b "a"] (O.qualified [O.tc (name ++ "K") [a]] (O.tc (name ++ "App") [f,a]))
