module Cost.Translate.Output.Modules
    ( moduleD
    , importD
    , hideImport
    ) where

import Cost.Translate.Output.Ast (Decl, rdr)
import GHC hiding (Pat, Stmt)
import GHC.Types.Name.Occurrence (varName)

moduleD :: String -> [LImportDecl GhcPs] -> [Decl] -> HsModule GhcPs
moduleD name =
    HsModule
        (XModulePs noAnn EpNoLayout Nothing Nothing)
        (Just (noLocA (mkModuleName name)))
        Nothing

importD :: String -> Maybe String -> LImportDecl GhcPs
importD name alias =
    noLocA
        ( (simpleImportDecl (mkModuleName name))
            { ideclQualified = maybe NotQualified (const QualifiedPre) alias
            , ideclAs = fmap (noLocA . mkModuleName) alias
            }
        )

hideImport :: String -> [String] -> LImportDecl GhcPs
hideImport name hidden =
    noLocA
        ( (simpleImportDecl (mkModuleName name))
            { ideclImportList =
                Just
                    ( EverythingBut
                    , noLocA
                        [ noLocA
                            ( IEVar
                                noAnn
                                ( noLocA
                                    ( IEName
                                        noExtField
                                        (noLocA (rdr varName item))
                                    )
                                )
                                Nothing
                            )
                        | item <- hidden
                        ]
                    )
            }
        )
