module Cost.Translate.Output.Types
    ( t
    , tapp
    , tc
    , promoted
    , listT
    , tupleT
    , promotedList
    , numT
    , strT
    , charT
    , kinded
    , parT
    , arrow
    , arrows
    , equalT
    ) where

import Cost.Translate.Output.Ast (Ty, nameParts, rdr)
import Data.Char (isUpper)
import GHC hiding (Pat, Stmt)
import GHC.Data.FastString (fsLit)
import GHC.Types.Basic (appPrec, funPrec, opPrec)
import GHC.Types.Name.Occurrence (dataName, tcName, tvName)
import GHC.Types.SourceText (SourceText (..))

t :: String -> Ty
t "()" = tupleT []
t name = noLocA (HsTyVar noAnn NotPromoted (noLocA (rdr namespace name)))
    where
        namespace = case snd (nameParts name) of
            first : _ | isUpper first || first `elem` "([~:" -> tcName
            _ -> tvName

tapp :: Ty -> [Ty] -> Ty
tapp =
    foldl
        ( \f x ->
            mkHsAppTy
                (parenthesizeHsType opPrec f)
                (parenthesizeHsType appPrec x)
        )

tc :: String -> [Ty] -> Ty
tc = tapp . t

promoted :: String -> [Ty] -> Ty
promoted name =
    tapp (noLocA (HsTyVar noAnn IsPromoted (noLocA (rdr dataName name))))

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

kinded :: Ty -> Ty -> Ty
kinded ty kind = noLocA (HsKindSig noAnn ty kind)

parT :: Ty -> Ty
parT = noLocA . HsParTy noAnn

arrow :: Ty -> Ty -> Ty
arrow a z =
    noLocA
        ( HsFunTy
            noExtField
            (HsUnrestrictedArrow noAnn)
            (parenthesizeHsType funPrec a)
            z
        )

arrows :: [Ty] -> Ty -> Ty
arrows = flip (foldr arrow)

equalT :: Ty -> Ty -> Ty
equalT a z = noLocA (HsOpTy noAnn NotPromoted a (noLocA (rdr tcName "~")) z)
