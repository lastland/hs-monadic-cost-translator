module Cost.Translate.Frontend.Gates
    ( checkImport
    , checkDeclaration
    , bindingError
    , locatedAt
    , checkRecursiveValues
    , checkSourceExtensions
    ) where

import Control.Monad (unless, when)
import Cost.Translate.Frontend.Syntax (arity)
import GHC
import GHC.Driver.Session (xopt)
import GHC.LanguageExtensions.Type qualified as Extension
import GHC.Types.Basic (RecFlag (..))
import GHC.Utils.Outputable
    ( SDocContext (sdocSuppressUniques)
    , defaultSDocContext
    , ppr
    , showSDocOneLine
    , showSDocUnsafe
    )

checkImport :: LImportDecl GhcRn -> Either String ()
checkImport located =
    unless
        ( moduleNameString (unLoc (ideclName (unLoc located)))
            `elem` ["Prelude", "Memorist.Example.NewtonRaphson"]
        )
        $ locatedAt (getLocA located) (Left "unsupported import")

checkDeclaration :: LHsDecl GhcPs -> Either String ()
checkDeclaration located = case unLoc located of
    ValD {} -> Right ()
    SigD {} -> Right ()
    TyClD _ DataDecl {tcdDataDefn = defn} | null (dd_derivs defn) -> Right ()
    TyClD _ DataDecl {} ->
        Left
            ( showSDocUnsafe (ppr (getLocA located))
                ++ ": source deriving clauses are unsupported"
            )
    _ -> locatedAt (getLocA located) (Left "unsupported declaration")

bindingError :: LHsBind GhcRn -> Either String a
bindingError located =
    Left
        (showSDocUnsafe (ppr (getLocA located)) ++ ": " ++ detail)
    where
        detail = case unLoc located of
            PatBind {pat_lhs = pat} ->
                "unsupported pattern binding "
                    ++ showSDocOneLine
                        defaultSDocContext {sdocSuppressUniques = True}
                        (ppr pat)
                    ++ "; only named value bindings are supported"
            _ -> "unsupported binding form: " ++ showSDocUnsafe (ppr located)

locatedAt :: SrcSpan -> Either String a -> Either String a
locatedAt srcSpan =
    either (Left . ((showSDocUnsafe (ppr srcSpan) ++ ": ") ++)) Right

checkRecursiveValues :: (RecFlag, [LHsBind GhcRn]) -> Either String ()
checkRecursiveValues (Recursive, bindings@(binding : _))
    | any isValue bindings =
        locatedAt (getLocA binding) $
            Left "recursive value bindings are unsupported"
    where
        isValue (L _ (FunBind _ _ matches)) = arity matches == 0
        isValue _ = False
checkRecursiveValues _ = Right ()

checkSourceExtensions :: FilePath -> ModSummary -> Either String ()
checkSourceExtensions file summary =
    when
        ( any
            (`xopt` ms_hspp_opts summary)
            [Extension.Strict, Extension.StrictData, Extension.RebindableSyntax]
        )
        ( Left
            ( file
                ++ ":1: Strict, StrictData and RebindableSyntax\
                   \ are unsupported"
            )
        )
