module Cost.Translate.Output.Ast
    ( Expr
    , Ty
    , Pat
    , Decl
    , Stmt
    , Binder
    , rdr
    , nameParts
    ) where

import Data.Char (isUpper)
import GHC hiding (Pat, Stmt)
import GHC.Types.Name.Occurrence (NameSpace, mkOccName)
import GHC.Types.Name.Reader (mkRdrQual, mkRdrUnqual)

type Expr = LHsExpr GhcPs
type Ty = LHsType GhcPs
type Pat = LPat GhcPs
type Decl = LHsDecl GhcPs
type Stmt = ExprLStmt GhcPs
type Binder = LHsTyVarBndr () GhcPs

rdr :: NameSpace -> String -> RdrName
rdr ns text = case nameParts text of
    (Just qualifier, occ) -> mkRdrQual qualifier (mkOccName ns occ)
    (Nothing, occ) -> mkRdrUnqual (mkOccName ns occ)

nameParts :: String -> (Maybe ModuleName, String)
nameParts text = case break (== '.') text of
    (qual@(first : _), '.' : occ)
        | isUpper first ->
            (Just (mkModuleName qual), occ)
    _ -> (Nothing, text)
