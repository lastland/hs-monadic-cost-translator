module Cost.Translate.Sharing (constrainedVariables, specializedSharing, sharingType, nativeSharing, nativeSharingSupport, sharingPaths, sharingSupport) where

import GHC
import GHC.Builtin.Types (liftedTypeKind, listTyCon, maybeTyCon)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, splitFunTy_maybe, splitTyConApp_maybe, splitAppTy_maybe, getTyVar_maybe, isPredTy, mkTyVarTy, substTyWith)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypes)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Types.Var (setVarName, tyVarKind)
import GHC.Types.Name (getOccString, mkInternalName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (getUnique)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Kinds (renderBinder, visibleArguments)
import Cost.Translate.Validation (scalarType)
import Data.List (minimumBy, nubBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Cost.Translate.Output as O

constrainedVariables :: Type -> [TyVar]
constrainedVariables ty = [v | v <- vars, v `elemVarSet` tyCoVarsOfTypes predicates]
  where
    (vars, body) = splitForAllTyVars ty
    predicates = takeWhile isPredTy (map scaledThing (fst (splitFunTys body)))

specializedSharing :: [TyVar] -> Type -> [Maybe Type] -> Maybe [Type]
specializedSharing scopeVariables ty uses
    | [variable] <- vars
    , tyVarKind variable `eqType` liftedTypeKind
    , variable `elem` constrainedVariables ty
    , all (isPredTy . scaledThing) arguments
    , Just resultVariable <- resultParameter result >>= getTyVar_maybe
    , resultVariable == variable
    , not (null uses) = do
        types <- sequence uses
        let distinct = nubBy eqType types
        if all closedResult types then Just distinct else case distinct of
            [value] | Just scoped <- resultParameter value >>= getTyVar_maybe
                    , scoped `elem` scopeVariables -> Just distinct
            _ -> Nothing
    | otherwise = Nothing
  where
    (vars, body) = splitForAllTyVars ty
    (arguments, result) = splitFunTys body
    resultParameter value = case maybeElement result of
        Just _ -> maybeElement value
        Nothing -> Just value
    closedResult value = maybe False closedScalar (resultParameter value)
    closedScalar value = isEmptyVarSet (tyCoVarsOfType value) && scalarType value
    maybeElement value = case splitTyConApp_maybe value of
        Just (constructor, [element]) | constructor == maybeTyCon -> Just element
        _ -> Nothing

sharingType :: Map.Map Name String -> Type -> ([TyVar], Type)
sharingType renamings ty = (renamed, substTyWith vars (map mkTyVarTy renamed) body)
  where
    (vars, body) = splitForAllTyVars ty
    renamed = map rename vars
    rename variable = case Map.lookup (getName variable) renamings of
        Nothing -> variable
        Just occurrence -> setVarName variable (mkInternalName (getUnique variable)
            (mkTyVarOcc occurrence) (nameSrcSpan (getName variable)))

nativeSharing :: [TyCon] -> Map.Map Name String -> Type -> Either String (Maybe (O.Ty, O.Expr))
nativeSharing datatypes renamings ty
    | null vars || not (all ((`eqType` liftedTypeKind) . tyVarKind) vars) || not (null arguments) = Right Nothing
    | otherwise = case mapper result of
        Nothing -> Right Nothing
        Just convert -> do
            let (renamed, renamedBody) = sharingType renamings ty
            quantified <- mapM (renderBinder datatypes) renamed
            rendered <- renderType datatypes (snd (splitFunTys renamedBody))
            pure (Just (O.forallT quantified (O.tc "M.MemoristM" [rendered]), convert))
  where
    (vars, body) = splitForAllTyVars ty
    (arguments, result) = splitFunTys body
    mapper value
        | not (any (`elemVarSet` tyCoVarsOfType value) vars) = Just (O.v "id")
        | Just variable <- getTyVar_maybe value, variable `elem` vars = Just (O.v "Void.absurd")
        | Just (constructor, [element]) <- splitTyConApp_maybe value, constructor == listTyCon =
            O.call "mapListT" . (:[]) <$> mapper element
        | otherwise = Nothing

nativeSharingSupport :: Bool -> [O.Decl]
nativeSharingSupport lists =
    [ O.sig "mapThunk" (O.arrows [O.arrow a b, th a] (th b))
    , O.val "mapThunk" [O.p "convert", O.conP "M.Th" (map O.p ["name", "payload", "annotation"])]
        (O.call "M.Th" [O.v "name", O.call "convert" [O.v "payload"], O.v "annotation"])
    ] ++ if not lists then [] else
    [ O.sig "mapListT" (O.arrows [O.arrow a b, list a] (list b))
    , O.val "mapListT" (map O.p ["convert", "list"]) (O.caseE (O.v "list")
        [(O.conP "L.NilT" [], O.v "L.NilT"),
         (O.conP "L.ConsT" (map O.p ["headThunk", "tailThunk"]),
          O.call "L.ConsT" [O.call "mapThunk" [O.v "convert", O.v "headThunk"],
              O.call "mapThunk" [O.call "mapListT" [O.v "convert"], O.v "tailThunk"]])])
    ]
  where
    a = O.t "a"
    b = O.t "b"
    th value = O.tc "M.Th" [value]
    list value = O.tc "L.ListT" [value]

sharingPaths :: Type -> Either String (Maybe O.Ty)
sharingPaths ty = case constrainedVariables ty of
    [] -> Right Nothing
    vars -> Just . O.promotedList <$> mapM path vars
  where
    result = snd (splitFunTys (snd (splitForAllTyVars ty)))
    path v = case paths v result of
        [] -> Left ("cannot locate constrained type parameter " ++ getOccString v ++ " in shared value result")
        choices -> Right (O.promotedList [O.promoted step [] | step <- minimumBy (comparing length) choices])
    paths wanted t
        | Just v <- getTyVar_maybe t = if v == wanted then [[]] else map ("SharingKind":) (paths wanted (tyVarKind v))
        | Just (_,_,a,b) <- splitFunTy_maybe t = map ("SharingDomain":) (paths wanted a) ++ map ("SharingCodomain":) (paths wanted b)
        | Just (tc,args) <- splitTyConApp_maybe t =
            let visible = visibleArguments tc args in concat
                [map ((replicate (length visible-i-1) "SharingFunction" ++ ["SharingArgument"]) ++) (paths wanted a) | (i,a) <- zip [0..] visible]
        | Just (f,a) <- splitAppTy_maybe t = map ("SharingFunction":) (paths wanted f) ++ map ("SharingArgument":) (paths wanted a)
        | otherwise = []

sharingSupport :: Bool -> [O.Decl]
sharingSupport reflected =
    [ O.sig "sharedAt" (O.arrows [m (th a), th (O.t "()")] (th a))
    , O.val "sharedAt" (map O.p ["action","anchor"]) (evaluateAt (O.call "M.tname" [O.v "anchor"]))
    ] ++ if not reflected then [] else
    [ O.dataD "SharingStep" [] [(name,[]) | name <- ["SharingFunction","SharingArgument","SharingKind","SharingDomain","SharingCodomain"]]
    , O.kindSig "ShareProject" (O.arrow steps (O.forallT [O.b "k"] (O.arrow k typ)))
    , O.closedFamily "ShareProject" (map O.b ["path","value"])
        [([O.promotedList [],value], O.tc "Proxy" [value]),
         ([step "SharingFunction", O.tapp f [a]], project f),
         ([step "SharingArgument", O.tapp f [a]], project a),
         ([step "SharingKind", O.kinded value k], project k),
         ([step "SharingDomain", O.arrow a (O.t "b")], project a),
         ([step "SharingCodomain", O.arrow a (O.t "b")], project (O.t "b"))]
    , O.kindSig "ShareKeys" (O.arrows [O.listT steps,typ] typ)
    , O.closedFamily "ShareKeys" (map O.b ["paths","value"])
        [([O.promotedList [],value], O.t "()"),
         ([O.promoted ":" [O.t "path",rest],value], O.tupleT [O.tc "ShareProject" [O.t "path",value], O.tc "ShareKeys" [rest,value]])]
    , O.sig "sharedAtBy" (O.forallT (map O.b ["paths","a"]) (O.qualified [O.tc "Reflection.Typeable" [keys]]
        (O.arrows [O.tc "Proxy" [O.t "paths"],m (th a),th (O.t "()")] (th a))))
    , O.val "sharedAtBy" (map O.p ["_","action","anchor"]) $ O.letE
        [O.val "key" [] (O.call "sharingTypeKey" [O.call "Reflection.someTypeRep" [O.ann (O.v "Proxy") (O.tc "Proxy" [keys])]]),
         O.val "name" [] (O.caseE (O.call "M.tname" [O.v "anchor"])
            [(O.conP "M.ThName" [O.p "ancestry"], O.call "M.ThName" [O.op "NE.:|" (O.integer 0)
                (O.op ":" (O.integer (-1)) (O.op "++" (O.v "key") (O.op ":" (O.integer (-2)) (O.call "NE.toList" [O.v "ancestry"]))))])])]
        (evaluateAt (O.v "name"))
    , O.sig "sharingTypeKey" (O.arrow (O.t "Reflection.SomeTypeRep") (O.listT (O.t "Int")))
    , O.val "sharingTypeKey" [O.conP "Reflection.SomeTypeRep" [O.p "ty"]] $ O.caseE (O.v "ty")
        [(O.conP "Reflection.App" (map O.p ["f","a"]), O.op ":" (O.integer 0) (O.op "++" (keyOf "f") (keyOf "a"))),
         (O.conP "Reflection.Con'" (map O.p ["con","kinds"]), O.op ":" (O.integer 1)
            (O.op "++" (O.call "concatMap" [O.v "sharingString", O.list [O.call method [O.v "con"] | method <- ["Reflection.tyConPackage","Reflection.tyConModule","Reflection.tyConName"]]])
             (O.op ":" (O.call "length" [O.v "kinds"]) (O.call "concatMap" [O.v "sharingTypeKey",O.v "kinds"]))))]
    , O.sig "sharingString" (O.arrow (O.t "String") (O.listT (O.t "Int")))
    , O.val "sharingString" [O.p "text"] (O.op ":" (O.call "length" [O.v "text"]) (O.call "map" [O.v "Char.ord",O.v "text"]))
    ]
  where
    a = O.t "a"; f = O.t "f"; k = O.t "k"; value = O.t "value"; rest = O.t "rest"; typ = O.t "Type"
    steps = O.listT (O.t "SharingStep")
    step name = O.promoted ":" [O.promoted name [],rest]
    project ty = O.tc "ShareProject" [rest,ty]
    keys = O.tc "ShareKeys" [O.t "paths",a]
    th ty = O.tc "M.Th" [ty]
    m ty = O.tc "M.MemoristM" [ty]
    evaluateAt name = O.call "M.value" [O.call "M.eval" [O.v "action",name]]
    keyOf name = O.call "sharingTypeKey" [O.call "Reflection.SomeTypeRep" [O.v name]]
