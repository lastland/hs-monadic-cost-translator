module Cost.Translate.Datatypes.Generation
    ( generatedInstance
    , demandHelpers
    , arbitraryInstance
    , demandArbitraryInstance
    ) where

import Cost.Translate.Datatypes.Constructor
import Cost.Translate.Datatypes.Declaration
import Cost.Translate.Datatypes.DemandSupport
    ( budgetName
    , emptyShrink
    , generationName
    , shrinkName
    )
import Cost.Translate.Names (demandHelperName)
import Cost.Translate.Output qualified as O

generatedInstance :: Datatype -> O.Decl
generatedInstance datatype =
    inst
        datatype
        (generationConstraints datatype)
        (O.tc "GeneratedDemand" [targetType datatype])
        (map emitGen cs ++ map emitShrink cs ++ [emptyShrink | length cs > 1])
    where
        cs = datatypeConstructors datatype

demandHelpers :: Datatype -> [O.Decl]
demandHelpers datatype =
    [ O.sig
        genHelper
        (signature datatype ctxt generator)
    , O.val genHelper [] (O.v generationName)
    , O.sig
        shrinkHelper
        (signature datatype ctxt (O.arrows [target, demand] (O.listT demand)))
    , O.val shrinkHelper [] (O.v shrinkName)
    ]
    where
        ctxt = generationConstraints datatype
        target = targetType datatype
        demand = demandType datatype
        genHelper = demandHelperName "gen" (datatypeName datatype)
        shrinkHelper = demandHelperName "shrink" (datatypeName datatype)
        generator = O.arrows [O.t "Int", target] (O.tc "QC.Gen" [demand])

arbitraryInstance :: Datatype -> O.Decl
arbitraryInstance datatype =
    inst
        datatype
        (map (shape arbitraryClass) (typeVariables datatype))
        (O.tc arbitraryClass [generatedType datatype "D"])
        [ O.val
            "arbitrary"
            []
            (O.call "QC.sized" [O.lam [O.p budgetName] choices])
        , O.val "shrink" [O.p "_"] (O.list [])
        ]
    where
        cs = datatypeConstructors datatype
        choices = oneOf (map emitArbitrary cs)

demandArbitraryInstance :: Datatype -> O.Decl
demandArbitraryInstance datatype =
    inst
        datatype
        demandArb
        (O.tc "M.DemandArb" [targetType datatype])
        [ O.val
            "arbDemandFrom"
            [O.p "x"]
            (O.call "QC.sized" [O.sectionR generationName (O.v "x")])
        ]
    where
        demandArb =
            generationConstraints datatype
                ++ [ shape cls (O.tc "DemOf" [v])
                   | v <- typeVariables datatype
                   , cls <- [arbitraryClass, "Show"]
                   ]

generationConstraints :: Datatype -> [O.Ty]
generationConstraints datatype = constraints datatype "GeneratedK"

arbitraryClass :: String
arbitraryClass = "QC.Arbitrary"

emitGen :: Constructor -> O.Decl
emitGen c =
    O.val generationName [O.p budgetName, targetPattern] $
        withWitnesses
            (demandWitnesses c)
            ( applicative
                (demandCon c)
                [O.call "genField" [budget c, O.v x] | x <- fieldNames c]
            )
    where
        targetPattern = patternFor (targetCon c) (fieldNames c)

emitShrink :: Constructor -> O.Decl
emitShrink c =
    O.val
        shrinkName
        [ patternFor (targetCon c) (fieldNames c)
        , patternFor (demandCon c) (map fieldDemandName (fieldNames c))
        ]
        $ withWitnesses
            (demandWitnesses c)
            ( appendLists
                [ O.listComp
                    ( O.call
                        (demandCon c)
                        [ O.v (if x == y then "smaller" else fieldDemandName y)
                        | y <- fieldNames c
                        ]
                    )
                    [("smaller", shrunkField x)]
                | x <- fieldNames c
                ]
            )
    where
        shrunkField x = O.call "shrinkField" [O.v x, O.v (fieldDemandName x)]

fieldDemandName :: String -> String
fieldDemandName = (++ "d")

emitArbitrary :: Constructor -> O.Expr
emitArbitrary c =
    applicative
        (demandCon c)
        [O.call "arbitraryField" [budget c] | _ <- fieldNames c]

budget :: Constructor -> O.Expr
budget c =
    O.op
        "div"
        (O.v budgetName)
        (O.integer (fromIntegral (max 1 (length (fieldNames c)))))

appendLists :: [O.Expr] -> O.Expr
appendLists [] = O.list []
appendLists xs = foldr1 (O.op "++") xs

oneOf :: [O.Expr] -> O.Expr
oneOf [choice] = choice
oneOf choices = O.call "QC.oneof" [O.list choices]
