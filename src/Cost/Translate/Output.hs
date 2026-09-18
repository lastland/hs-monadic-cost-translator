-- | Small constructors for GHC's parsed output syntax. No source fragments or
-- parser calls live here: callers exchange GHC expressions, types and declarations.
module Cost.Translate.Output where

import GHC hiding (Pat, Stmt)
import GHC.Types.Basic (Origin(..), Boxity(..), TopLevelFlag(..), appPrec, opPrec, funPrec)
import GHC.Types.Var (Specificity(..))
import GHC.Types.Name.Occurrence (NameSpace, varName, dataName, tcName, tvName, mkOccName)
import GHC.Types.Name.Reader (mkRdrQual, mkRdrUnqual)
import GHC.Types.SourceText (mkIntegralLit, SourceText(..))
import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (fsLit)
import Data.Char (isUpper)

type Expr = LHsExpr GhcPs
type Ty = LHsType GhcPs
type Pat = LPat GhcPs
type Decl = LHsDecl GhcPs
type Stmt = ExprLStmt GhcPs
type Binder = LHsTyVarBndr () GhcPs

-- The strings accepted here are single names, never Haskell expressions.
rdr :: NameSpace -> String -> RdrName
rdr ns text = case nameParts text of
    (Just qualifier,occ) -> mkRdrQual qualifier (mkOccName ns occ)
    (Nothing,occ) -> mkRdrUnqual (mkOccName ns occ)

nameParts :: String -> (Maybe ModuleName,String)
nameParts text = case break (== '.') text of
    (qual@(first:_),'.':occ) | isUpper first -> (Just (mkModuleName qual),occ)
    _ -> (Nothing,text)

v :: String -> Expr
v name = noLocA (HsVar noExtField (noLocA (rdr namespace name)))
  where namespace = case occurrence of { first:_ | isUpper first || first `elem` ":(" -> dataName; _ -> varName }
        occurrence = snd (nameParts name)

app :: Expr -> [Expr] -> Expr
app = foldl (\f x -> mkHsApp (parenthesizeHsExpr opPrec f) (parenthesizeHsExpr appPrec x))

call :: String -> [Expr] -> Expr
call = app . v

ret :: Expr -> Expr
ret x = call "pure" [x]

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
op name a z = noLocA (OpApp noAnn (parenthesizeHsExpr appPrec a) (v name) (parenthesizeHsExpr appPrec z))

lam :: [Pat] -> Expr -> Expr
lam = mkHsLam

ifE :: Expr -> Expr -> Expr -> Expr
ifE c yes no = noLocA (mkHsIf c yes no noAnn)

caseE :: Expr -> [(Pat,Expr)] -> Expr
caseE subject branches = noLocA (HsCase noAnn subject (MG FromSource (noLocA
    [mkSimpleMatch CaseAlt [pat] body | (pat,body) <- branches])))

bind :: String -> Expr -> Stmt
bind name body = noLocA (BindStmt noAnn (p name) body)

thenS :: Expr -> Stmt
thenS = noLocA . mkBodyStmt

doE :: [Stmt] -> Expr -> Expr
doE [] body = body
doE statements body = noLocA (mkHsDo (DoExpr Nothing) (noLocA (statements ++ [noLocA (mkLastStmt final)])))
  where final = case unLoc body of { HsLet {} -> mkHsPar body; _ -> body }

letE :: [Decl] -> Expr -> Expr
letE [] body = body
letE declarations body = noLocA (HsLet noAnn (locals declarations) body)

ann :: Expr -> Ty -> Expr
ann body ty = mkHsPar (noLocA (ExprWithTySig noAnn subject (signature ty)))
  where subject = case unLoc body of { HsDo {} -> mkHsPar body; HsLet {} -> mkHsPar body; _ -> body }

p :: String -> Pat
p "_" = noLocA (WildPat noExtField)
p name = noLocA (VarPat noExtField (noLocA (rdr varName name)))

conP :: String -> [Pat] -> Pat
conP name ps = noLocA (ConPat noAnn (noLocA (rdr dataName name)) (PrefixCon [] (map (parenthesizePat appPrec) ps)))

t :: String -> Ty
t "()" = tupleT []
t name = noLocA (HsTyVar noAnn NotPromoted (noLocA (rdr namespace name)))
  where namespace = case snd (nameParts name) of { first:_ | isUpper first || first `elem` "([~:" -> tcName; _ -> tvName }

tapp :: Ty -> [Ty] -> Ty
tapp = foldl (\f x -> mkHsAppTy (parenthesizeHsType opPrec f) (parenthesizeHsType appPrec x))

tc :: String -> [Ty] -> Ty
tc = tapp . t

promoted :: String -> [Ty] -> Ty
promoted name = tapp (noLocA (HsTyVar noAnn IsPromoted (noLocA (rdr dataName name))))

listT :: Ty -> Ty
listT = noLocA . HsListTy noAnn

tupleT :: [Ty] -> Ty
tupleT = noLocA . HsTupleTy noAnn HsBoxedOrConstraintTuple

promotedList :: [Ty] -> Ty
promotedList = noLocA . HsExplicitListTy noAnn IsPromoted

numT :: Integer -> Ty
numT = noLocA . HsTyLit noExtField . HsNumTy NoSourceText

strT :: String -> Ty
strT = noLocA . HsTyLit noExtField . HsStrTy NoSourceText . fsLit

charT :: Char -> Ty
charT = noLocA . HsTyLit noExtField . HsCharTy NoSourceText

listComp :: Expr -> [(String,Expr)] -> Expr
listComp body generators = noLocA (mkHsDo ListComp (noLocA
    (map (uncurry bind) generators ++ [noLocA (mkLastStmt body)])))

kinded :: Ty -> Ty -> Ty
kinded ty kind = noLocA (HsKindSig noAnn ty kind)

b :: String -> Binder
b name = noLocA (UserTyVar noAnn () (noLocA (rdr tvName name)))

bk :: String -> Ty -> Binder
bk name kind = noLocA (KindedTyVar noAnn () (noLocA (rdr tvName name)) kind)

flagBinder :: flag -> Binder -> LHsTyVarBndr flag GhcPs
flagBinder flag (L loc binder) = L loc $ case binder of
    UserTyVar ext _ name -> UserTyVar ext flag name
    KindedTyVar ext _ name kind -> KindedTyVar ext flag name kind

forallT :: [Binder] -> Ty -> Ty
forallT [] body = body
forallT bs body = noLocA (HsForAllTy noExtField (HsForAllInvis noAnn (map (flagBinder SpecifiedSpec) bs)) body)

forallVisible :: [Binder] -> Ty -> Ty
forallVisible bs body = noLocA (HsForAllTy noExtField (HsForAllVis noAnn bs) body)

qualified :: [Ty] -> Ty -> Ty
qualified [] body = body
qualified predicates body = noLocA (HsQualTy noExtField (noLocA (map constraintPar predicates)) body)

constraintPar :: Ty -> Ty
constraintPar ty = case unLoc ty of { HsForAllTy {} -> parT ty; HsQualTy {} -> parT ty; _ -> ty }

parT :: Ty -> Ty
parT = noLocA . HsParTy noAnn

arrow :: Ty -> Ty -> Ty
arrow a z = noLocA (HsFunTy noExtField (HsUnrestrictedArrow noAnn) (parenthesizeHsType funPrec a) z)

arrows :: [Ty] -> Ty -> Ty
arrows = flip (foldr arrow)

equalT :: Ty -> Ty -> Ty
equalT a z = noLocA (HsOpTy noAnn NotPromoted a (noLocA (rdr tcName "~")) z)

signature :: Ty -> LHsSigWcType GhcPs
signature = mkHsWildCardBndrs . noLocA . mkHsImplicitSigType

sig :: String -> Ty -> Decl
sig name ty = noLocA (SigD noExtField (TypeSig noAnn [noLocA (rdr varName name)] (signature ty)))

val :: String -> [Pat] -> Expr -> Decl
val name ps body = noLocA (ValD noExtField (mkFunBind FromSource located
    [mkSimpleMatch (FunRhs located Prefix NoSrcStrict) ps body]))
  where located = noLocA (rdr varName name)

locals :: [Decl] -> HsLocalBinds GhcPs
locals declarations = HsValBinds noAnn (ValBinds NoAnnSortKey (listToBag [noLocA binding | L _ (ValD _ binding) <- declarations])
    [noLocA s | L _ (SigD _ s) <- declarations])

kindSig :: String -> Ty -> Decl
kindSig name ty = noLocA (KindSigD noExtField (StandaloneKindSig noAnn (noLocA (rdr tcName name)) (noLocA (mkHsImplicitSigType ty))))

typeSyn :: String -> [Binder] -> Ty -> Decl
typeSyn name bs body = noLocA (TyClD noExtField (SynDecl noAnn (noLocA (rdr tcName name)) (qtvs bs) Prefix body))

qtvs :: [Binder] -> LHsQTyVars GhcPs
qtvs = mkHsQTvs . map (flagBinder (HsBndrRequired noExtField))

familyEquation :: String -> [Ty] -> Ty -> TyFamInstEqn GhcPs
familyEquation name args = FamEqn noAnn (noLocA (rdr tcName name)) (HsOuterImplicit noExtField)
    (map (HsValArg noExtField . parenthesizeHsType appPrec) args) Prefix

closedFamily :: String -> [Binder] -> [([Ty],Ty)] -> Decl
closedFamily name bs equations = noLocA (TyClD noExtField (FamDecl noExtField
    (FamilyDecl noAnn (ClosedTypeFamily (Just [noLocA (familyEquation name args result) | (args,result) <- equations]))
        TopLevel (noLocA (rdr tcName name)) (qtvs bs) Prefix (noLocA (NoSig noExtField)) Nothing)))

assoc :: String -> [Ty] -> Ty -> Decl
assoc name args result = noLocA (InstD noExtField (TyFamInstD noExtField (TyFamInstDecl noAnn (familyEquation name args result))))

classD :: [Ty] -> String -> [Binder] -> [Decl] -> Decl
classD predicates name bs members = noLocA (TyClD noExtField (ClassDecl ([], EpNoLayout, NoAnnSortKey)
    (if null predicates then Nothing else Just (noLocA (map constraintPar predicates)))
    (noLocA (rdr tcName name)) (qtvs bs) Prefix []
    [noLocA s | L _ (SigD _ s) <- members]
    (listToBag [noLocA binding | L _ (ValD _ binding) <- members]) [] [] []))

instanceD :: Ty -> [Decl] -> Decl
instanceD ty members = noLocA (InstD noExtField (ClsInstD noExtField (ClsInstDecl (Nothing, [], NoAnnSortKey)
    (noLocA (mkHsImplicitSigType ty))
    (listToBag [noLocA binding | L _ (ValD _ binding) <- members])
    [noLocA s | L _ (SigD _ s) <- members]
    [noLocA equation | L _ (InstD _ (TyFamInstD _ equation)) <- members] [] Nothing)))

derivingD :: Ty -> Decl
derivingD ty = noLocA (DerivD noExtField (DerivDecl noAnn (signature ty) Nothing Nothing))

dataD :: String -> [Binder] -> [(String,[Ty])] -> Decl
dataD name bs constructors = dataWith name bs
    [constructor con (PrefixCon [] [HsScaled (HsUnrestrictedArrow noAnn) (parenthesizeHsType appPrec field) | field <- fields]) | (con,fields) <- constructors]

recordD :: String -> [(String,Ty)] -> Decl
recordD name fields = dataWith name [] [constructor name (RecCon (noLocA
    [noLocA (ConDeclField noAnn [noLocA (FieldOcc noExtField (noLocA (rdr varName field)))] ty Nothing) | (field,ty) <- fields]))]

constructor :: String -> HsConDeclH98Details GhcPs -> LConDecl GhcPs
constructor name details = noLocA (ConDeclH98 noAnn (noLocA (rdr dataName name)) False [] Nothing details Nothing)

dataWith :: String -> [Binder] -> [LConDecl GhcPs] -> Decl
dataWith name bs cons = noLocA (TyClD noExtField (DataDecl noAnn (noLocA (rdr tcName name)) (qtvs bs) Prefix
    (HsDataDefn noExtField Nothing Nothing Nothing (DataTypeCons False cons) [])))

moduleD :: String -> [LImportDecl GhcPs] -> [Decl] -> HsModule GhcPs
moduleD name = HsModule (XModulePs noAnn EpNoLayout Nothing Nothing)
    (Just (noLocA (mkModuleName name))) Nothing

importD :: String -> Maybe String -> LImportDecl GhcPs
importD name alias = noLocA ((simpleImportDecl (mkModuleName name))
    { ideclQualified = maybe NotQualified (const QualifiedPre) alias
    , ideclAs = fmap (noLocA . mkModuleName) alias })

hideImport :: String -> [String] -> LImportDecl GhcPs
hideImport name hidden = noLocA ((simpleImportDecl (mkModuleName name))
    { ideclImportList = Just (EverythingBut, noLocA [noLocA (IEVar noAnn (noLocA (IEName noExtField (noLocA (rdr varName item)))) Nothing) | item <- hidden]) })
