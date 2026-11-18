module Cost.Translate.Kinds.Lifting (lifting) where

import Cost.Translate.Output qualified as O

lifting :: String -> String -> String -> [O.Decl]
lifting name family base =
    [ O.kindSig
        requirement
        (O.forallVisible [O.b kindName] (O.arrow k constraint))
    , O.closedFamily
        requirement
        (map O.b [kindName, argumentName])
        [ ([O.t "Type", a], O.tc base [a])
        , ([O.arrow k l, f], O.tc function [f])
        , ([k, a], O.t "()")
        ]
    , O.kindSig
        kindClass
        (O.forallT [O.b kindName] (O.arrow k constraint))
    , O.typeSyn kindClass [O.bk argumentName k] (O.tc requirement [k, a])
    , O.kindSig
        application
        ( O.forallT
            (map O.b [kindName, resultName])
            (O.arrows [O.arrow k l, k] constraint)
        )
    , O.classD predicates application (map O.b [functionName, argumentName]) []
    , O.instanceD (O.qualified predicates (O.tc application [f, a])) []
    , O.kindSig
        function
        ( O.forallT
            (map O.b [kindName, resultName])
            (O.arrow (O.arrow k l) constraint)
        )
    , O.classD [lifted] function [O.b functionName] []
    , O.instanceD (O.qualified [lifted] (O.tc function [f])) []
    ]
    where
        requirement = name ++ "Req"
        kindClass = name ++ "K"
        application = name ++ "App"
        function = name ++ "F"
        argumentName = "a"
        functionName = "f"
        kindName = "k"
        resultName = "l"
        a = O.t argumentName
        f = O.t functionName
        k = O.t kindName
        l = O.t resultName
        constraint = O.t "Constraint"
        predicates = liftingPredicates kindClass family f a
        lifted = liftingConstraint argumentName kindClass application f a

liftingPredicates :: String -> String -> O.Ty -> O.Ty -> [O.Ty]
liftingPredicates kindClass family f a =
    [ O.tc kindClass [O.tapp f [a]]
    , O.equalT
        (O.tc family [O.tapp f [a]])
        (O.tc family [f, O.tc family [a]])
    ]

liftingConstraint :: String -> String -> String -> O.Ty -> O.Ty -> O.Ty
liftingConstraint argumentName kindClass application f a =
    O.forallT
        [O.b argumentName]
        (O.qualified [O.tc kindClass [a]] (O.tc application [f, a]))
