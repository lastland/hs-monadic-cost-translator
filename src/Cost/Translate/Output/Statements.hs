module Cost.Translate.Output.Statements
    ( bind
    , thenS
    , doE
    , letE
    , listComp
    ) where

import Cost.Translate.Output.Ast (Decl, Expr, Stmt)
import Cost.Translate.Output.Declarations (locals)
import Cost.Translate.Output.Patterns (p)
import GHC hiding (Pat, Stmt)

bind :: String -> Expr -> Stmt
bind name body = noLocA (BindStmt noAnn (p name) body)

thenS :: Expr -> Stmt
thenS = noLocA . mkBodyStmt

doE :: [Stmt] -> Expr -> Expr
doE [] body = body
doE statements (L _ (HsDo _ (DoExpr Nothing) inner)) =
    noLocA (mkHsDo (DoExpr Nothing) (noLocA (statements ++ unLoc inner)))
doE statements body =
    noLocA
        ( mkHsDo
            (DoExpr Nothing)
            (noLocA (statements ++ [noLocA (mkLastStmt final)]))
        )
    where
        final = case unLoc body of HsLet {} -> mkHsPar body; _ -> body

letE :: [Decl] -> Expr -> Expr
letE [] body = body
letE declarations (L _ (HsDo _ (DoExpr Nothing) statements)) =
    noLocA
        ( mkHsDo
            (DoExpr Nothing)
            ( noLocA
                ( noLocA (LetStmt noAnn (locals declarations))
                    : unLoc statements
                )
            )
        )
letE declarations body = noLocA (HsLet noAnn (locals declarations) body)

listComp :: Expr -> [(String, Expr)] -> Expr
listComp body generators =
    noLocA
        ( mkHsDo
            ListComp
            ( noLocA
                (map (uncurry bind) generators ++ [noLocA (mkLastStmt body)])
            )
        )
