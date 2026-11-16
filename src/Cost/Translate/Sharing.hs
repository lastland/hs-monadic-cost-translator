module Cost.Translate.Sharing (constrainedVariables, sharingPaths, sharingSupport) where

import GHC
import GHC.Core.Type (splitForAllTyVars, splitFunTys, splitFunTy_maybe, splitTyConApp_maybe, splitAppTy_maybe, getTyVar_maybe, isPredTy)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.FVs (tyCoVarsOfTypes)
import GHC.Types.Var.Set (elemVarSet)
import GHC.Types.Var (tyVarKind)
import GHC.Types.Name (getOccString)
import Cost.Translate.Kinds (visibleArguments)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Cost.Translate.Output as O

constrainedVariables :: Type -> [TyVar]
constrainedVariables ty = [v | v <- vars, v `elemVarSet` tyCoVarsOfTypes predicates]
  where
    (vars, body) = splitForAllTyVars ty
    predicates = takeWhile isPredTy (map scaledThing (fst (splitFunTys body)))

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
