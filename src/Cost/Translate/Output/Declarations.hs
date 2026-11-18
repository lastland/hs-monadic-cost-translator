module Cost.Translate.Output.Declarations
    ( sig
    , val
    , locals
    , kindSig
    , typeSyn
    , familyEquation
    , closedFamily
    , assoc
    , classD
    , instanceD
    , derivingD
    , dataD
    , recordD
    , constructor
    , dataWith
    ) where

import Cost.Translate.Output.Ast (Binder, Decl, Expr, Pat, Ty, rdr)
import Cost.Translate.Output.Binders (constraintPar, qtvs, signature)
import GHC hiding (Pat, Stmt)
import GHC.Data.Bag (listToBag)
import GHC.Types.Basic (Origin (..), TopLevelFlag (..), appPrec)
import GHC.Types.Name.Occurrence (dataName, tcName, varName)

sig :: String -> Ty -> Decl
sig name ty =
    noLocA
        ( SigD
            noExtField
            (TypeSig noAnn [noLocA (rdr varName name)] (signature ty))
        )

val :: String -> [Pat] -> Expr -> Decl
val name ps body =
    noLocA
        ( ValD
            noExtField
            ( mkFunBind
                FromSource
                located
                [mkSimpleMatch (FunRhs located Prefix NoSrcStrict) ps body]
            )
        )
    where
        located = noLocA (rdr varName name)

locals :: [Decl] -> HsLocalBinds GhcPs
locals declarations =
    HsValBinds
        noAnn
        ( ValBinds
            NoAnnSortKey
            (listToBag [noLocA binding | L _ (ValD _ binding) <- declarations])
            [noLocA s | L _ (SigD _ s) <- declarations]
        )

kindSig :: String -> Ty -> Decl
kindSig name ty =
    noLocA
        ( KindSigD
            noExtField
            ( StandaloneKindSig
                noAnn
                (noLocA (rdr tcName name))
                (noLocA (mkHsImplicitSigType ty))
            )
        )

typeSyn :: String -> [Binder] -> Ty -> Decl
typeSyn name bs body =
    noLocA
        ( TyClD
            noExtField
            (SynDecl noAnn (noLocA (rdr tcName name)) (qtvs bs) Prefix body)
        )

familyEquation :: String -> [Ty] -> Ty -> TyFamInstEqn GhcPs
familyEquation name args =
    FamEqn
        noAnn
        (noLocA (rdr tcName name))
        (HsOuterImplicit noExtField)
        (map (HsValArg noExtField . parenthesizeHsType appPrec) args)
        Prefix

closedFamily :: String -> [Binder] -> [([Ty], Ty)] -> Decl
closedFamily name bs equations =
    noLocA
        ( TyClD
            noExtField
            ( FamDecl
                noExtField
                ( FamilyDecl
                    noAnn
                    ( ClosedTypeFamily
                        ( Just
                            [ noLocA (familyEquation name args result)
                            | (args, result) <- equations
                            ]
                        )
                    )
                    TopLevel
                    (noLocA (rdr tcName name))
                    (qtvs bs)
                    Prefix
                    (noLocA (NoSig noExtField))
                    Nothing
                )
            )
        )

assoc :: String -> [Ty] -> Ty -> Decl
assoc name args result =
    noLocA
        ( InstD
            noExtField
            ( TyFamInstD
                noExtField
                (TyFamInstDecl noAnn (familyEquation name args result))
            )
        )

classD :: [Ty] -> String -> [Binder] -> [Decl] -> Decl
classD predicates name bs members =
    noLocA
        ( TyClD
            noExtField
            ( ClassDecl
                ([], EpNoLayout, NoAnnSortKey)
                ( if null predicates
                    then Nothing
                    else Just (noLocA (map constraintPar predicates))
                )
                (noLocA (rdr tcName name))
                (qtvs bs)
                Prefix
                []
                [noLocA s | L _ (SigD _ s) <- members]
                (listToBag [noLocA binding | L _ (ValD _ binding) <- members])
                []
                []
                []
            )
        )

instanceD :: Ty -> [Decl] -> Decl
instanceD ty members =
    noLocA
        ( InstD
            noExtField
            ( ClsInstD
                noExtField
                ( ClsInstDecl
                    (Nothing, [], NoAnnSortKey)
                    (noLocA (mkHsImplicitSigType ty))
                    ( listToBag
                        [noLocA binding | L _ (ValD _ binding) <- members]
                    )
                    [noLocA s | L _ (SigD _ s) <- members]
                    [ noLocA equation
                    | L _ (InstD _ (TyFamInstD _ equation)) <- members
                    ]
                    []
                    Nothing
                )
            )
        )

derivingD :: Ty -> Decl
derivingD ty =
    noLocA (DerivD noExtField (DerivDecl noAnn (signature ty) Nothing Nothing))

dataD :: String -> [Binder] -> [(String, [Ty])] -> Decl
dataD name bs constructors =
    dataWith
        name
        bs
        [ constructor
            con
            ( PrefixCon
                []
                [ HsScaled
                    (HsUnrestrictedArrow noAnn)
                    (parenthesizeHsType appPrec field)
                | field <- fields
                ]
            )
        | (con, fields) <- constructors
        ]

recordD :: String -> [(String, Ty)] -> Decl
recordD name fields =
    dataWith
        name
        []
        [ constructor
            name
            ( RecCon
                ( noLocA
                    [ noLocA
                        ( ConDeclField
                            noAnn
                            [ noLocA
                                ( FieldOcc
                                    noExtField
                                    (noLocA (rdr varName field))
                                )
                            ]
                            ty
                            Nothing
                        )
                    | (field, ty) <- fields
                    ]
                )
            )
        ]

constructor :: String -> HsConDeclH98Details GhcPs -> LConDecl GhcPs
constructor name details =
    noLocA
        ( ConDeclH98
            noAnn
            (noLocA (rdr dataName name))
            False
            []
            Nothing
            details
            Nothing
        )

dataWith :: String -> [Binder] -> [LConDecl GhcPs] -> Decl
dataWith name bs cons =
    noLocA
        ( TyClD
            noExtField
            ( DataDecl
                noAnn
                (noLocA (rdr tcName name))
                (qtvs bs)
                Prefix
                ( HsDataDefn
                    noExtField
                    Nothing
                    Nothing
                    Nothing
                    (DataTypeCons False cons)
                    []
                )
            )
        )
