module Cost.Translate.Frontend.Signatures
    ( translatedSignatureUsing
    , translatedSignatureAs
    , translatedSignatureAsUsing
    , monadicType
    ) where

import Control.Monad (unless)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Frontend.Syntax (builtin)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing (constrainedVariables, sharingType)
import Data.Map.Strict qualified as Map
import GHC
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (isPredTy, splitFunTys, splitTyConApp_maybe)
import GHC.Types.Name (getOccString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

translatedSignatureUsing ::
    Bool -> Bool -> [TyCon] -> Int -> Type -> Either String O.Ty
translatedSignatureUsing reflected factory datatypes count =
    translatedSignatureAs reflected wrapResult datatypes count
    where
        wrapResult resultType
            | count == 0 = O.arrows [O.t "()" | factory] (thunkType resultType)
            | otherwise = monadicType resultType

translatedSignatureAs ::
    Bool -> (O.Ty -> O.Ty) -> [TyCon] -> Int -> Type -> Either String O.Ty
translatedSignatureAs = translatedSignatureAsUsing Map.empty

translatedSignatureAsUsing ::
    Map.Map Name String ->
    Bool ->
    (O.Ty -> O.Ty) ->
    [TyCon] ->
    Int ->
    Type ->
    Either String O.Ty
translatedSignatureAsUsing names reflected wrap datatypes count ty = do
    let (vars, body) = sharingType names ty
        (scaledArgs, result) = splitFunTys body
        (constraints, args) = span isPredTy (map scaledThing scaledArgs)
    quantified <- mapM (renderBinder datatypes) vars
    sourceConstraints <- concat <$> mapM translatedConstraint constraints
    let renderedConstraints =
            sourceConstraints
                ++ [ O.tc
                        "Reflection.Typeable"
                        [O.t variable]
                   | reflected
                   , v <- constrainedVariables ty
                   , let variable =
                            Map.findWithDefault
                                (getOccString v)
                                (getName v)
                                names
                   ]
    unless
        (length args == count)
        (Left "function-valued result has no validated calling convention")
    argTypes <- mapM (renderType datatypes) args
    resultType <- renderType datatypes result
    pure
        $ O.forallT quantified
        $ O.qualified renderedConstraints
        $ O.arrows (map thunkType argTypes) (wrap resultType)

translatedConstraint :: Type -> Either String [O.Ty]
translatedConstraint ty = case splitTyConApp_maybe ty of
    Just (tc, [arg])
        | builtin (getName tc) "GHC.Internal.Data.Foldable" "Foldable" ->
            (\a -> [O.tc "FoldableT" [a]]) <$> renderType [] arg
    Just (tc, args)
        | getOccString tc
            `elem` [ "Num"
                   , "Eq"
                   , "Ord"
                   , "Fractional"
                   , "Floating"
                   , "Integral"
                   , "Real"
                   , "RealFrac"
                   , "RealFloat"
                   , "Enum"
                   , "Bounded"
                   ] -> do
            rendered <- mapM (renderType []) args
            let scalar = getOccString tc `elem` ["Eq", "Ord"]
            pure
                ( O.tc (getOccString tc) rendered
                    : [O.tc "Scalar" [a] | scalar, a <- rendered]
                )
    _ -> Left ("unsupported type constraint: " ++ showSDocUnsafe (ppr ty))

monadicType :: O.Ty -> O.Ty
monadicType resultType = O.tc "M.MemoristM" [resultType]

thunkType :: O.Ty -> O.Ty
thunkType resultType = O.tc "M.Th" [resultType]
