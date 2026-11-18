module Cost.Translate.Datatypes.DemandSupport
    ( generationSupport
    , generatedDemand
    , generationName
    , shrinkName
    , budgetName
    , field
    , emptyShrink
    ) where

import Cost.Translate.Output qualified as O

generationSupport :: [O.Decl]
generationSupport =
    demandClass
        : fieldSupport
        ++ [ O.instanceD
                (generatedDemand (O.t ty))
                [O.val generationName [O.p "_"] (O.v "pure")]
           | ty <- ["Int", "Integer", "Bool", "Float", "Double", "Char", "()"]
           ]

demandClass :: O.Decl
demandClass =
    O.classD
        [O.tc "M.Demand" [a]]
        "GeneratedDemand"
        [O.b "a"]
        [ O.sig generationName (O.arrows [int, a] (gen (dem a)))
        , O.sig shrinkName (O.arrows [a, dem a] (O.listT (dem a)))
        , emptyShrink
        ]

fieldSupport :: [O.Decl]
fieldSupport =
    [ O.sig
        genFieldName
        (O.qualified [generatedDemand a] generatorType)
    , O.val
        genFieldName
        (map O.p [budgetName, thunkName])
        (bounded (O.call generationName [smaller, O.call "M.tval" [thunk]]))
    , O.sig
        "shrinkField"
        ( O.qualified
            [generatedDemand a]
            (O.arrows [th a, thd (dem a)] (O.listT (thd (dem a))))
        )
    , O.val
        "shrinkField"
        [O.p thunkName]
        (O.call "QC.liftShrink" [O.call shrinkName [O.call "M.tval" [thunk]]])
    , O.sig
        "arbitraryField"
        (O.qualified [O.tc "QC.Arbitrary" [a]] (O.arrow int (gen (thd a))))
    , O.val
        "arbitraryField"
        [O.p budgetName]
        (bounded (O.call "QC.resize" [smaller, O.v "QC.arbitrary"]))
    ]
    where
        generatorType = O.arrows [int, th a] (gen (thd (dem a)))
        smaller = O.op "-" (O.v budgetName) (O.integer 2)
        thunkName = "t"
        thunk = O.v thunkName

generatedDemand :: O.Ty -> O.Ty
generatedDemand ty = O.tc "GeneratedDemand" [ty]

generationName, shrinkName, budgetName, genFieldName :: String
generationName = "genDemandBudget"
shrinkName = "shrinkDemandCompatible"
budgetName = "n"
genFieldName = "genField"

field :: O.Expr -> String -> O.Expr
field size name = O.call genFieldName [size, O.v name]

emptyShrink :: O.Decl
emptyShrink = O.val shrinkName (replicate 2 (O.p "_")) (O.list [])

a, int :: O.Ty
a = O.t "a"
int = O.t "Int"

dem, th, thd, gen :: O.Ty -> O.Ty
dem ty = O.tc "M.Dem" [ty]
th ty = O.tc "M.Th" [ty]
thd ty = O.tc "M.ThD" [ty]
gen ty = O.tc "QC.Gen" [ty]

bounded :: O.Expr -> O.Expr
bounded recurse =
    O.ifE
        (O.op "<=" (O.v budgetName) (O.integer 1))
        (O.ret (O.v "M.Skip"))
        (O.call "QC.liftArbitrary" [recurse])
