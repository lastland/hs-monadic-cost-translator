module Cost.Translate.Output.Binders
    ( b
    , bk
    , flagBinder
    , forallT
    , forallVisible
    , qualified
    , constraintPar
    , signature
    , qtvs
    ) where

import Cost.Translate.Output.Ast (Binder, Ty, rdr)
import Cost.Translate.Output.Types (parT)
import GHC hiding (Pat, Stmt)
import GHC.Types.Name.Occurrence (tvName)
import GHC.Types.Var (Specificity (..))

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
forallT bs body =
    noLocA
        ( HsForAllTy
            noExtField
            (HsForAllInvis noAnn (map (flagBinder SpecifiedSpec) bs))
            body
        )

forallVisible :: [Binder] -> Ty -> Ty
forallVisible bs body =
    noLocA (HsForAllTy noExtField (HsForAllVis noAnn bs) body)

qualified :: [Ty] -> Ty -> Ty
qualified [] body = body
qualified predicates body =
    noLocA (HsQualTy noExtField (noLocA (map constraintPar predicates)) body)

constraintPar :: Ty -> Ty
constraintPar ty = case unLoc ty of
    HsForAllTy {} -> parT ty
    HsQualTy {} -> parT ty
    _ -> ty

signature :: Ty -> LHsSigWcType GhcPs
signature = mkHsWildCardBndrs . noLocA . mkHsImplicitSigType

qtvs :: [Binder] -> LHsQTyVars GhcPs
qtvs = mkHsQTvs . map (flagBinder (HsBndrRequired noExtField))
