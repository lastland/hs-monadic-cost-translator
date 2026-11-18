module Cost.Translate.Validation (validateComparisons, scalarType, scalarSupport) where

import Control.Monad (unless)
import Cost.Translate.Output qualified as O
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import GHC.Builtin.Types
    ( boolTy
    , charTy
    , doubleTy
    , floatTy
    , intTy
    , integerTy
    , unitTy
    )
import GHC.Core qualified as Core
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type
    ( Type
    , getTyVar_maybe
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    )
import GHC.Core.Utils (exprType)
import GHC.Types.Name
    ( Name
    , getName
    , getOccString
    , nameModule_maybe
    , nameSrcSpan
    )
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

validateComparisons :: Core.CoreProgram -> Either String ()
validateComparisons = mapM_ binding
    where
        binding (Core.NonRec name expr) = walk (showSDocUnsafe (ppr (nameSrcSpan (getName name)))) expr
        binding (Core.Rec bindings) = mapM_ (binding . uncurry Core.NonRec) bindings
        walk location expr = do
            case expr of
                Core.Type _ -> pure ()
                Core.Coercion _ -> pure ()
                _ ->
                    mapM_
                        (check location . scaledThing)
                        (fst (splitFunTys (snd (splitForAllTyVars (exprType expr)))))
            case expr of
                Core.App f a -> walk location f >> walk location a
                Core.Lam _ body -> walk location body
                Core.Let bindings body -> binding bindings >> walk location body
                Core.Case scrutinee _ _ alts ->
                    walk location scrutinee >> mapM_ (\(Core.Alt _ _ rhs) -> walk location rhs) alts
                Core.Cast body _ -> walk location body
                Core.Tick _ body -> walk location body
                _ -> pure ()
        check location ty = case splitTyConApp_maybe ty of
            Just (tc, [argument])
                | getOccString tc `elem` ["Eq", "Ord"]
                , ghcName (getName tc) ->
                    unless
                        (scalarType argument)
                        ( Left
                            ( location
                                ++ ": comparisons support only strict scalar operands; found "
                                ++ showSDocUnsafe (ppr argument)
                            )
                        )
            _ -> pure ()

scalarType :: Type -> Bool
scalarType ty =
    isJust (getTyVar_maybe ty)
        || any
            (eqType ty)
            [intTy, integerTy, boolTy, floatTy, doubleTy, charTy, unitTy]

ghcName :: Name -> Bool
ghcName name =
    maybe
        False
        (("GHC." `isPrefixOf`) . moduleNameString . moduleName)
        (nameModule_maybe name)

scalarSupport :: [O.Decl]
scalarSupport =
    O.classD [] "Scalar" [O.b "a"] []
        : [ O.instanceD (O.tc "Scalar" [O.t ty]) []
          | ty <- ["Int", "Integer", "Bool", "Float", "Double", "Char", "()"]
          ]
