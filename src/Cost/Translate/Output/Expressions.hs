module Cost.Translate.Output.Expressions
    ( v
    , app
    , call
    , ret
    , integer
    , overLit
    , string
    , char
    , list
    , tuple
    , op
    , sectionR
    , lam
    , ifE
    , caseE
    , ann
    , typeApp
    ) where

import Cost.Translate.Output.Ast (Expr, Pat, Ty, nameParts, rdr)
import Cost.Translate.Output.Binders (signature)
import Data.Char (isUpper)
import GHC hiding (Pat, Stmt)
import GHC.Types.Basic (Boxity (..), Origin (..), appPrec, opPrec)
import GHC.Types.Name.Occurrence (dataName, varName)
import GHC.Types.SourceText (SourceText (..), mkIntegralLit)

v :: String -> Expr
v name = noLocA (HsVar noExtField (noLocA (rdr namespace name)))
    where
        namespace = case occurrence of
            first : _ | isUpper first || first `elem` ":(" -> dataName
            _ -> varName
        occurrence = snd (nameParts name)

app :: Expr -> [Expr] -> Expr
app =
    foldl
        ( \f x ->
            mkHsApp
                (parenthesizeHsExpr opPrec f)
                (parenthesizeHsExpr appPrec x)
        )

call :: String -> [Expr] -> Expr
call = app . v

ret :: Expr -> Expr
ret x = call "pure" [x]

typeApp :: Expr -> Ty -> Expr
typeApp expression ty =
    noLocA
        ( HsAppType
            noAnn
            expression
            (mkHsWildCardBndrs (parenthesizeHsType appPrec ty))
        )

integer :: Integer -> Expr
integer = overLit . ol_val . mkHsIntegral . mkIntegralLit

overLit :: OverLitVal -> Expr
overLit value = noLocA (HsOverLit noExtField (OverLit noExtField value))

string :: String -> Expr
string = noLocA . HsLit noExtField . mkHsString

char :: Char -> Expr
char = noLocA . HsLit noExtField . HsChar NoSourceText

list :: [Expr] -> Expr
list = noLocA . ExplicitList noAnn

tuple :: [Expr] -> Expr
tuple xs = noLocA (ExplicitTuple noAnn (map (Present noExtField) xs) Boxed)

op :: String -> Expr -> Expr -> Expr
op name a z =
    noLocA
        ( OpApp
            noAnn
            (parenthesizeHsExpr opPrec a)
            (v name)
            (parenthesizeHsExpr opPrec z)
        )

sectionR :: String -> Expr -> Expr
sectionR name z =
    mkHsPar
        ( noLocA
            (SectionR noExtField (v name) (parenthesizeHsExpr opPrec z))
        )

lam :: [Pat] -> Expr -> Expr
lam = mkHsLam

ifE :: Expr -> Expr -> Expr -> Expr
ifE c yes no = noLocA (mkHsIf c yes no noAnn)

caseE :: Expr -> [(Pat, Expr)] -> Expr
caseE subject branches =
    noLocA
        ( HsCase
            noAnn
            subject
            ( MG
                FromSource
                ( noLocA
                    [mkSimpleMatch CaseAlt [pat] body | (pat, body) <- branches]
                )
            )
        )

ann :: Expr -> Ty -> Expr
ann body ty = mkHsPar (noLocA (ExprWithTySig noAnn subject (signature ty)))
    where
        subject = case unLoc body of
            HsDo {} -> mkHsPar body
            HsLet {} -> mkHsPar body
            _ -> body
