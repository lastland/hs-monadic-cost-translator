module Cost.Translate.Datatypes.BuiltinDemand (builtinDemandSupport) where

import Cost.Translate.Datatypes.Constructor (applicative, patternFor)
import Cost.Translate.Datatypes.DemandSupport
import Cost.Translate.Output qualified as O

builtinDemandSupport :: [O.Decl]
builtinDemandSupport = [listDemand, maybeDemand, pairDemand]

listDemand :: O.Decl
listDemand =
    O.instanceD
        (O.qualified [generatedDemand a] (generatedDemand (O.tc "L.ListT" [a])))
        [ O.val generationName [O.p "_", O.conP "L.NilT" []] nilDemand
        , O.val
            generationName
            [O.p budgetName, patternFor "L.ConsT" [headName, tailName]]
            (applicative "L.ConsD" [field half headName, field half tailName])
        , shrinkTwo "L.ConsT" "L.ConsD" headName tailName "dh" "dt"
        , emptyShrink
        ]
    where
        headName = "h"
        nilDemand = O.ret (O.v "L.NilD")

maybeDemand :: O.Decl
maybeDemand =
    O.instanceD
        (O.qualified [generatedDemand a] target)
        [ O.val
            generationName
            [O.p "_", O.conP "M.NothingT" []]
            (O.ret (O.v "M.NothingD"))
        , O.val
            generationName
            [O.p budgetName, justPattern]
            (applicative justDemand [field (O.v budgetName) tailName])
        , O.val
            shrinkName
            [justPattern, patternFor justDemand [demandName]]
            (O.listComp smaller [("s", shrunk tailName demandName)])
        , emptyShrink
        ]
    where
        target = generatedDemand (O.tc "M.MaybeT" [a])
        justPattern = patternFor "M.JustT" [tailName]
        justDemand = "M.JustD"
        demandName = "d"
        smaller = O.call justDemand [O.v "s"]

pairDemand :: O.Decl
pairDemand =
    O.instanceD
        ( O.qualified
            [generatedDemand a, generatedDemand b]
            (generatedDemand (O.tc pairTarget [a, b]))
        )
        [ O.val
            generationName
            [O.p budgetName, patternFor pairTarget [firstField, secondField]]
            (applicative "M.PairD" fields)
        , shrinkTwo pairTarget "M.PairD" firstField secondField "da" "db"
        ]
    where
        firstField = "a"
        secondField = "b"
        fields = [field half firstField, field half secondField]
        pairTarget = "M.PairT"

a, b :: O.Ty
a = O.t "a"
b = O.t "b"

tailName :: String
tailName = "t"

half :: O.Expr
half = O.op "div" (O.v budgetName) (O.integer 2)

shrinkTwo :: String -> String -> String -> String -> String -> String -> O.Decl
shrinkTwo con demand x y dx dy =
    O.val
        shrinkName
        [patternFor con [x, y], patternFor demand [dx, dy]]
        $ O.op
            "++"
            ( O.listComp
                (O.call demand [O.v smaller, O.v dy])
                [(smaller, shrunk x dx)]
            )
            ( O.listComp
                (O.call demand [O.v dx, O.v smaller])
                [(smaller, shrunk y dy)]
            )
    where
        smaller = "d"

shrunk :: String -> String -> O.Expr
shrunk value demand = O.call "shrinkField" [O.v value, O.v demand]
