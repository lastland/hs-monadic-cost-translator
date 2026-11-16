module Cost.Translate.Datatypes (renderType, emitDatatypes, emitDatatypeSections, constructors, selectors, checkConversionExports) where

import GHC
import GHC.Types.Basic (Boxity(..))
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import GHC.Core.Type (getTyVar_maybe, splitTyConApp_maybe, splitAppTy_maybe, isNumLitTy, isStrLitTy, isCharLitTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon (isPromotedDataCon)
import GHC.Data.FastString (unpackFS)
import GHC.Core.DataCon (dataConOrigArgTys, dataConExTyCoVars, dataConTheta, dataConImplBangs, isBanged)
import GHC.Types.FieldLabel (flSelector)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Cost.Translate.Names (generatedName, demandHelperName, sourceConstructor)
import Cost.Translate.Kinds (visibleArguments, visibleVariables, renderBinder, kindSupport)
import qualified Cost.Translate.Output as O
import Control.Monad (unless, when)
import Data.List (nub, nubBy)
import qualified Data.Set as Set

checkConversionExports :: [Name] -> [TyCon] -> Either String ()
checkConversionExports exported = mapM_ check
  where
    visible = Set.fromList exported
    check tc = do
        mapM_ (requireExport "constructor" . getName) (tyConDataCons tc)
        requireExport "type" (getName tc)
    requireExport kind name = unless (name `Set.member` visible) $
        Left (showSDocUnsafe (ppr (nameSrcSpan name)) ++ ": generated conversions require the source module to export " ++ kind ++ " " ++ getOccString name)

renderType :: [TyCon] -> Type -> Either String O.Ty
renderType = render "T"

render :: String -> [TyCon] -> Type -> Either String O.Ty
render suffix locals ty
    | Just v <- getTyVar_maybe ty = Right (O.t (getOccString v))
    | Just n <- isNumLitTy ty = Right (O.numT n)
    | Just s <- isStrLitTy ty = Right (O.strT (unpackFS s))
    | Just c <- isCharLitTy ty = Right (O.charT c)
    | Just (tc,args) <- splitTyConApp_maybe ty = do
        rendered <- mapM (render suffix locals) (visibleArguments tc args)
        case () of
            _ | isPromotedDataCon tc -> pure (O.promoted (promoted tc) rendered)
              | tc == listTyCon -> pure (O.tc (builtin "[]" "L.List") rendered)
              | tc == maybeTyCon -> pure (O.tc (builtin "Maybe" "M.Maybe") rendered)
              | tc == tupleTyCon Boxed 2 -> pure (O.tc (builtin "(,)" "M.Pair") rendered)
              | tc `elem` locals -> pure (O.tc (if null suffix then sourceConstructor (getName tc) else generatedName suffix (getName tc)) rendered)
              | null args, getOccString tc `elem` ["Int","Integer","Bool","Float","Double","Char","()"] -> pure (O.t (getOccString tc))
              | otherwise -> unsupported
    | Just (f,a) <- splitAppTy_maybe ty = O.tapp <$> render suffix locals f <*> ((:[]) <$> render suffix locals a)
    | otherwise = unsupported
  where
    builtin original transformed = if null suffix then original else transformed ++ suffix
    promoted tc = case getOccString tc of
        "[]" -> "[]"
        ":" -> ":"
        tuple@('(':_) -> tuple
        identifier -> qualifier tc ++ identifier
    qualifier tc = if any ((== nameModule_maybe (getName tc)) . nameModule_maybe . getName) locals
        then "Source." else "Prelude."
    unsupported = Left ("unsupported type representation: " ++ showSDocUnsafe (ppr ty))

constructors :: [TyCon] -> [(Name, String, Int, [Name])]
constructors types = [(getName c, generatedName "T" (getName c), length (dataConOrigArgTys c), map flSelector (dataConFieldLabels c)) | t <- types, c <- tyConDataCons t]

selectors :: [TyCon] -> [(Name, String)]
selectors types = [(flSelector f, generatedName "M" (flSelector f)) | t <- types, c <- tyConDataCons t, f <- dataConFieldLabels c]

applicationWitnesses :: String -> String -> [TyCon] -> [Type] -> Either String [O.Ty]
applicationWitnesses cls suffix locals fields = mapM witness (nubBy same (concatMap applications fields))
  where
    same (f,a) (g,b) = eqType f g && eqType a b
    witness (f,a) = do
        f' <- render suffix locals f
        a' <- render suffix locals a
        pure (O.tc cls [f',a'])
    applications ty
        | Just (tc,args) <- splitTyConApp_maybe ty = concatMap applications (visibleArguments tc args)
        | Just (f,a) <- splitAppTy_maybe ty = applications f ++ applications a ++ [(f,a)]
        | otherwise = []

withWitnesses :: [O.Ty] -> O.Expr -> O.Expr
withWitnesses constraints body = foldr wrap body constraints
  where wrap c rest = O.call "withConstraint" [O.ann (O.v "Proxy") (O.tc "Proxy" [c]),rest]

data Constructor = Constructor
    { sourceCon :: String, targetCon :: String, demandCon :: String
    , fieldNames :: [String], targetFields :: [O.Ty], demandFields :: [O.Ty]
    , reprWitnesses :: [O.Ty], demandWitnesses :: [O.Ty]
    }

constructorInfo :: [TyCon] -> DataCon -> Either String Constructor
constructorInfo locals con = do
    unless (null (dataConExTyCoVars con) && null (dataConTheta con)) (Left "existential/constrained constructors are unsupported")
    when (any isBanged (dataConImplBangs con)) (Left "strict fields are unsupported")
    let fields = map scaledThing (dataConOrigArgTys con)
    Constructor (sourceConstructor (getName con)) (generatedName "T" (getName con)) (generatedName "D" (getName con))
        ["x" ++ show i | i <- [1 :: Int .. length fields]]
        <$> mapM (render "T" locals) fields <*> mapM (render "D" locals) fields
        <*> applicationWitnesses "ReprApp" "" locals fields
        <*> applicationWitnesses "DemandApp" "T" locals fields

emitDatatypes :: [TyCon] -> Either String [O.Decl]
emitDatatypes types = do
    (common, support, declarations) <- emitDatatypeSections types
    pure (common ++ support ++ declarations)

emitDatatypeSections :: [TyCon] -> Either String ([O.Decl], [O.Decl], [O.Decl])
emitDatatypeSections types = do
    support <- kindSupport types
    selectorDecls <- selectorDefinitions types
    declarations <- concat <$> mapM located types
    pure (charSupport ++ demandSupport, support, selectorDecls ++ declarations)
  where
    located tc = either (Left . ((showSDocUnsafe (ppr (nameSrcSpan (getName tc))) ++ ": ") ++)) Right (emit tc)
    emit tc = do
        when (isNewTyCon tc) (Left "newtype declarations are unsupported")
        cs <- mapM (constructorInfo types) (tyConDataCons tc)
        binders <- mapM (renderBinder types) (visibleVariables tc)
        quantified <- mapM (renderBinder types) (tyConTyVars tc)
        let vs = map (O.t . getOccString) (visibleVariables tc)
            name suffix = generatedName suffix (getName tc)
            typ suffix = O.tc (name suffix) vs
            original = O.tc (sourceConstructor (getName tc)) vs
            mapped family = [O.tc family [v] | v <- vs]
            demand = O.tc (name "D") (mapped "DemOf")
            constraints cls = [O.tc cls [v] | v <- vs]
            signature ctxt body = O.forallT quantified (O.qualified ctxt body)
            inst ctxt target = O.instanceD (signature ctxt target)
            shape cls ty = O.tc "ShapeK" [O.t cls,ty]
            demandArb = constraints "GeneratedK" ++ [shape cls (O.tc "DemOf" [v]) | v <- vs, cls <- ["QC.Arbitrary","Show"]]
            reprDemand = concat [[O.tc "ReprK" [v], O.tc "DemandK" [O.tc "RepOf" [v]]] | v <- vs]
            reprDemandArb = concat [[O.tc "ReprK" [v], O.tc "GeneratedK" [O.tc "RepOf" [v]]] ++
                [shape cls (O.tc "DemOf" [O.tc "RepOf" [v]]) | cls <- ["QC.Arbitrary","Show"]] | v <- vs]
            dataDecl suffix fields con = O.dataD (name suffix) binders
                [(con c, [O.tc (if suffix == "T" then "M.Th" else "M.ThD") [f] | f <- fields c]) | c <- cs]
            derived cls = O.derivingD (signature (map (shape cls) vs) (O.tc cls [typ "D"]))
        pure
            [dataDecl "T" targetFields targetCon,
             inst (constraints "ReprK") (O.tc "M.Repr" [original])
                (O.assoc "Rep" [original] (O.tc (name "T") (mapped "RepOf")) : map emitRepr cs ++ map emitAbstract cs),
             dataDecl "D" demandFields demandCon, derived "Eq", derived "Show",
             inst (constraints "DemandK") (O.tc "M.Demand" [typ "T"])
                (O.assoc "Dem" [typ "T"] demand : map emitFull cs ++ map emitUsage cs ++ [O.val "usage" [O.p "_",O.p "_"] (O.v "M.empty") | length cs > 1]),
             inst (constraints "DemandK") (O.tc "M.LBoundDemand" [typ "T"]) (map emitMinimum cs),
             inst (constraints "GeneratedK") (O.tc "GeneratedDemand" [typ "T"])
                (map emitGen cs ++ map emitShrink cs ++ [O.val "shrinkDemandCompatible" [O.p "_",O.p "_"] (O.list []) | length cs > 1]),
             O.sig (demandHelperName "gen" (getName tc)) (signature (constraints "GeneratedK") (O.arrows [O.t "Int",typ "T"] (O.tc "QC.Gen" [demand]))),
             O.val (demandHelperName "gen" (getName tc)) [] (O.v "genDemandBudget"),
             O.sig (demandHelperName "shrink" (getName tc)) (signature (constraints "GeneratedK") (O.arrows [typ "T",demand] (O.listT demand))),
             O.val (demandHelperName "shrink" (getName tc)) [] (O.v "shrinkDemandCompatible"),
             inst (map (shape "QC.Arbitrary") vs) (O.tc "QC.Arbitrary" [typ "D"])
                [O.val "arbitrary" [] (O.call "QC.sized" [O.lam [O.p "n"] (oneOf (map emitArbitrary cs))]), O.val "shrink" [O.p "_"] (O.list [])],
             inst demandArb (O.tc "M.DemandArb" [typ "T"])
                [O.val "arbDemandFrom" [O.p "x"] (O.call "QC.sized" [O.sectionR "genDemandBudget" (O.v "x")])],
             inst reprDemand (O.tc "M.ReprDemand" [original]) [],
             inst reprDemandArb (O.tc "M.ReprDemandArb" [original]) []]

patternFor :: String -> [String] -> O.Pat
patternFor con = O.conP con . map O.p

emitRepr, emitAbstract, emitFull, emitUsage, emitMinimum, emitGen, emitShrink :: Constructor -> O.Decl
emitRepr c = O.val "reprM" [patternFor (sourceCon c) (fieldNames c)] $ withWitnesses (reprWitnesses c)
    (applicative (targetCon c) [O.call "M.lazy" [O.call "M.reprM" [O.v x]] | x <- fieldNames c])
emitAbstract c = O.val "abstract" [patternFor (targetCon c) (fieldNames c)] $ withWitnesses (reprWitnesses c)
    (O.call (sourceCon c) [O.call "M.abstract" [O.call "M.tval" [O.v x]] | x <- fieldNames c])
emitFull c = O.val "fullDemand" [patternFor (targetCon c) (fieldNames c)] $ withWitnesses (demandWitnesses c)
    (O.call (demandCon c) [O.call "M.fullDemand" [O.v x] | x <- fieldNames c])
emitUsage c = O.val "usage" [patternFor (targetCon c) (fieldNames c),patternFor (demandCon c) (map (++"d") (fieldNames c))] $ withWitnesses (demandWitnesses c)
    (foldr (O.op "M.union") (O.v "M.empty") [O.call "M.usage" [O.v x,O.v (x ++ "d")] | x <- fieldNames c])
emitMinimum c = O.val "minDemand" [minimumPattern] (O.call (demandCon c) (replicate fieldCount (O.v "M.Skip")))
  where
    fieldCount = length (fieldNames c)
    minimumPattern = if fieldCount == 0 then O.conP (targetCon c) [] else O.conRecordP (targetCon c)
emitGen c = O.val "genDemandBudget" [O.p "n",patternFor (targetCon c) (fieldNames c)] $ withWitnesses (demandWitnesses c)
    (applicative (demandCon c) [O.call "genField" [budget c,O.v x] | x <- fieldNames c])
emitShrink c = O.val "shrinkDemandCompatible" [patternFor (targetCon c) (fieldNames c),patternFor (demandCon c) (map (++"d") (fieldNames c))] $ withWitnesses (demandWitnesses c)
    (appendLists [O.listComp (O.call (demandCon c) [O.v (if x == y then "smaller" else y ++ "d") | y <- fieldNames c])
        [("smaller",O.call "shrinkField" [O.v x,O.v (x ++ "d")])] | x <- fieldNames c])

emitArbitrary :: Constructor -> O.Expr
emitArbitrary c = applicative (demandCon c) [O.call "arbitraryField" [budget c] | _ <- fieldNames c]

budget :: Constructor -> O.Expr
budget c = O.op "div" (O.v "n") (O.integer (fromIntegral (max 1 (length (fieldNames c)))))

applicative :: String -> [O.Expr] -> O.Expr
applicative con [] = O.ret (O.v con)
applicative con (field:fields) = foldl (O.op "<*>") (O.op "<$>" (O.v con) field) fields

appendLists :: [O.Expr] -> O.Expr
appendLists [] = O.list []
appendLists xs = foldr1 (O.op "++") xs

oneOf :: [O.Expr] -> O.Expr
oneOf [choice] = choice
oneOf choices = O.call "QC.oneof" [O.list choices]

selectorDefinitions :: [TyCon] -> Either String [O.Decl]
selectorDefinitions types = concat <$> mapM forType types
  where
    forType tc = do
        quantified <- mapM (renderBinder types) (tyConTyVars tc)
        let vs = map (O.t . getOccString) (visibleVariables tc)
            recordType = O.tc (generatedName "T" (getName tc)) vs
            fields = nub [(flSelector field, generatedName "M" (flSelector field)) |
                con <- tyConDataCons tc, field <- dataConFieldLabels con]
        concat <$> mapM (emitSelector quantified recordType tc) fields
    emitSelector quantified recordType tc (name,generated) = do
        fieldTy <- maybe (Left "missing record selector field type") Right (selectorType tc name)
        rendered <- render "T" types fieldTy
        let signature = O.forallT quantified (O.arrow (O.tc "M.Th" [recordType]) (O.tc "M.MemoristM" [rendered]))
            cases = [(patternFor (generatedName "T" (getName con))
                        [if i == index then fieldName else "_" | i <- [0..fieldCount-1]],
                      O.call "M.force" [O.v fieldName]) |
                     con <- tyConDataCons tc,
                     let labels = map flSelector (dataConFieldLabels con),
                     (index,field) <- zip [0 :: Int ..] labels, field == name,
                     let fieldCount = length (dataConOrigArgTys con),
                     let fieldName = "field" ++ show index]
            fallback = [(O.p "_",O.call "error" [O.string "source record selector failure"]) | not (total tc name)]
            body = O.doE
                [O.thenS (O.ann (O.v "M.tick") (O.tc "M.MemoristM" [O.t "()"])),
                 O.bind "recordValue" (O.call "M.force" [O.v "recordThunk"])]
                (O.caseE (O.v "recordValue") (cases ++ fallback))
        pure [O.sig generated signature, O.val generated [O.p "recordThunk"] body]
    selectorType tc name = case
        [scaledThing ty | con <- tyConDataCons tc,
         (field,ty) <- zip (map flSelector (dataConFieldLabels con)) (dataConOrigArgTys con), field == name] of
        ty:_ -> Just ty
        [] -> Nothing
    total tc name = all (elem name . map flSelector . dataConFieldLabels) (tyConDataCons tc)

charSupport :: [O.Decl]
charSupport =
    [ O.instanceD (O.tc "M.Repr" [O.t "Char"])
        [O.assoc "Rep" [O.t "Char"] (O.t "Char"),
         O.val "reprM" [] (O.v "pure"),
         O.val "abstract" [O.p "c"] (O.v "c")]
    , O.instanceD (O.tc "M.Demand" [O.t "Char"])
        [O.assoc "Dem" [O.t "Char"] (O.t "Char"),
         O.val "fullDemand" [O.p "c"] (O.v "c")]
    , O.instanceD (O.tc "M.LBoundDemand" [O.t "Char"])
        [O.val "minDemand" [O.p "c"] (O.v "c")]
    , O.instanceD (O.tc "M.DemandArb" [O.t "Char"])
        [O.val "arbDemandFrom" [] (O.v "pure")]
    ]

demandSupport :: [O.Decl]
demandSupport =
    [ O.classD [O.tc "M.Demand" [a]] "GeneratedDemand" [O.b "a"]
        [O.sig "genDemandBudget" (O.arrows [O.t "Int",a] (gen (dem a))),
         O.sig "shrinkDemandCompatible" (O.arrows [a,dem a] (O.listT (dem a))), emptyShrink]
    , O.sig "genField" (O.qualified [gd a] (O.arrows [O.t "Int",th a] (gen (thd (dem a)))))
    , O.val "genField" (map O.p ["n","t"]) (bounded (O.call "genDemandBudget" [smaller, O.call "M.tval" [O.v "t"]]))
    , O.sig "shrinkField" (O.qualified [gd a] (O.arrows [th a,thd (dem a)] (O.listT (thd (dem a)))))
    , O.val "shrinkField" [O.p "t"] (O.call "QC.liftShrink" [O.call "shrinkDemandCompatible" [O.call "M.tval" [O.v "t"]]])
    , O.sig "arbitraryField" (O.qualified [O.tc "QC.Arbitrary" [a]] (O.arrow (O.t "Int") (gen (thd a))))
    , O.val "arbitraryField" [O.p "n"] (bounded (O.call "QC.resize" [smaller,O.v "QC.arbitrary"]))
    ] ++ [O.instanceD (gd (O.t ty)) [O.val "genDemandBudget" [O.p "_"] (O.v "pure")] | ty <- ["Int","Integer","Bool","Float","Double","Char","()"]] ++
    [ O.instanceD (O.qualified [gd a] (gd (O.tc "L.ListT" [a])))
        [O.val "genDemandBudget" [O.p "_",O.conP "L.NilT" []] (O.ret (O.v "L.NilD")),
         O.val "genDemandBudget" [O.p "n",patternFor "L.ConsT" ["h","t"]] (applicative "L.ConsD" [field half "h",field half "t"]),
         shrinkTwo "L.ConsT" "L.ConsD" "h" "t" "dh" "dt", emptyShrink]
    , O.instanceD (O.qualified [gd a] (gd (O.tc "M.MaybeT" [a])))
        [O.val "genDemandBudget" [O.p "_",O.conP "M.NothingT" []] (O.ret (O.v "M.NothingD")),
         O.val "genDemandBudget" [O.p "n",patternFor "M.JustT" ["t"]] (applicative "M.JustD" [field (O.v "n") "t"]),
         O.val "shrinkDemandCompatible" [patternFor "M.JustT" ["t"],patternFor "M.JustD" ["d"]]
            (O.listComp (O.call "M.JustD" [O.v "s"]) [("s",O.call "shrinkField" [O.v "t",O.v "d"])]), emptyShrink]
    , O.instanceD (O.qualified [gd a,gd b] (gd (O.tc "M.PairT" [a,b])))
        [O.val "genDemandBudget" [O.p "n",patternFor "M.PairT" ["a","b"]] (applicative "M.PairD" [field half "a",field half "b"]),
         shrinkTwo "M.PairT" "M.PairD" "a" "b" "da" "db"]
    ]
  where
    a = O.t "a"; b = O.t "b"
    gd ty = O.tc "GeneratedDemand" [ty]
    dem ty = O.tc "M.Dem" [ty]
    th ty = O.tc "M.Th" [ty]
    thd ty = O.tc "M.ThD" [ty]
    gen ty = O.tc "QC.Gen" [ty]
    smaller = O.op "-" (O.v "n") (O.integer 2)
    half = O.op "div" (O.v "n") (O.integer 2)
    bounded recurse = O.ifE (O.op "<=" (O.v "n") (O.integer 1)) (O.ret (O.v "M.Skip")) (O.call "QC.liftArbitrary" [recurse])
    field size name = O.call "genField" [size,O.v name]
    emptyShrink = O.val "shrinkDemandCompatible" [O.p "_",O.p "_"] (O.list [])
    shrinkTwo con demand x y dx dy = O.val "shrinkDemandCompatible" [patternFor con [x,y],patternFor demand [dx,dy]] $
        O.op "++" (O.listComp (O.call demand [O.v "d",O.v dy]) [("d",O.call "shrinkField" [O.v x,O.v dx])])
                  (O.listComp (O.call demand [O.v dx,O.v "d"]) [("d",O.call "shrinkField" [O.v y,O.v dy])])
