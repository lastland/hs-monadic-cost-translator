module Cost.Translate.Datatypes.CharSupport (charSupport) where

import Cost.Translate.Output qualified as O

charSupport :: [O.Decl]
charSupport =
    [ O.instanceD
        (O.tc "M.Repr" [char])
        [ O.assoc "Rep" [char] char
        , O.val "reprM" [] (O.v "pure")
        , O.val "abstract" [characterPattern] character
        ]
    , O.instanceD
        (O.tc "M.Demand" [char])
        [ O.assoc "Dem" [char] char
        , O.val "fullDemand" [characterPattern] character
        ]
    , O.instanceD
        (O.tc "M.LBoundDemand" [char])
        [O.val "minDemand" [characterPattern] character]
    , O.instanceD
        (O.tc "M.DemandArb" [char])
        [O.val "arbDemandFrom" [] (O.v "pure")]
    ]
    where
        char = O.t "Char"
        characterPattern = O.p "c"
        character = O.v "c"
