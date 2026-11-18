module Cost.Translate.Kinds.Requirements
    ( generatedRequirements
    , shapeRequirements
    ) where

import Cost.Translate.Output qualified as O

generatedRequirements :: [O.Decl]
generatedRequirements =
    [ O.kindSig
        requirement
        (O.forallVisible [O.b kindName] (O.arrow k constraint))
    , O.closedFamily
        requirement
        (map O.b [kindName, argumentName])
        [ ([typeKind, a], O.tc "GeneratedDemand" [a])
        , ([O.arrow k l, f], O.tc function [f])
        , ([k, a], O.t "()")
        ]
    , O.kindSig kindClass (O.forallT [O.b kindName] (O.arrow k constraint))
    , O.classD context kindClass [O.bk argumentName k] []
    , O.instanceD (O.qualified context (O.tc kindClass [O.kinded a k])) []
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
        requirement = "GeneratedReq"
        kindClass = "GeneratedK"
        function = "GeneratedF"
        a = O.t argumentName
        k = O.t kindName
        f = O.t functionName
        l = O.t resultName
        context = [O.tc "DemandK" [a], O.tc requirement [k, a]]
        lifted =
            O.forallT
                [O.b argumentName]
                ( O.qualified
                    [O.tc kindClass [a]]
                    (O.tc kindClass [O.tapp f [a]])
                )

shapeRequirements :: [O.Decl]
shapeRequirements =
    [ O.kindSig
        requirement
        ( O.arrow
            classKind
            (O.forallVisible [O.b kindName] (O.arrow k constraint))
        )
    , O.closedFamily
        requirement
        (map O.b [className, kindName, argumentName])
        [ ([c, typeKind, a], O.tapp c [a])
        , ([c, O.arrow k l, f], O.tc function [c, f])
        , ([c, k, a], O.t "()")
        ]
    , O.kindSig
        kindClass
        ( O.arrow
            classKind
            (O.forallT [O.b kindName] (O.arrow k constraint))
        )
    , O.classD context kindClass [O.b className, O.bk argumentName k] []
    , O.instanceD (O.qualified context (O.tc kindClass [c, O.kinded a k])) []
    , O.kindSig
        function
        ( O.arrow
            classKind
            ( O.forallT
                (map O.b [kindName, resultName])
                (O.arrow (O.arrow k l) constraint)
            )
        )
    , O.classD [lifted] function (map O.b [className, functionName]) []
    , O.instanceD (O.qualified [lifted] (O.tc function [c, f])) []
    ]
    where
        requirement = "ShapeReq"
        kindClass = "ShapeK"
        function = "ShapeF"
        className = "c"
        a = O.t argumentName
        k = O.t kindName
        l = O.t resultName
        f = O.t functionName
        c = O.t className
        classKind = O.arrow typeKind constraint
        context = [O.tc requirement [c, k, a]]
        lifted = shapeLift kindClass c f a

shapeLift :: String -> O.Ty -> O.Ty -> O.Ty -> O.Ty
shapeLift kindClass c f a =
    O.forallT
        [O.b argumentName]
        ( O.qualified
            [O.tc kindClass [c, a]]
            (O.tc kindClass [c, O.tapp f [a]])
        )

argumentName, kindName, resultName, functionName :: String
argumentName = "a"
kindName = "k"
resultName = "l"
functionName = "f"

constraint, typeKind :: O.Ty
constraint = O.t "Constraint"
typeKind = O.t "Type"
