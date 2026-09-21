-- | The scalar-comparison gate over desugared Core.
--
-- 'validateComparisons' walks the Core program and requires every
-- instantiated @Eq@ / @Ord@ constraint to have strict scalar operands. The
-- reason: the runtime's @Eq@ / @Ord@ instances on thunked structures
-- compare thunk metadata, not source elements, so structured operands are
-- rejected before any candidate is generated. Desugared types are used for
-- this validation only; instrumentation always uses renamed syntax.
--
-- 'scalarSupport' emits the generated @Scalar@ class and its instances,
-- keeping polymorphic exported primitives in the supported domain even
-- when a handwritten client instantiates them outside the source module.
module Cost.Translate.Validation (validateComparisons, scalarSupport) where

import qualified GHC.Core as Core
import GHC.Core.Utils (exprType)
import GHC.Core.Type (Type, splitForAllTyVars, splitFunTys, splitTyConApp_maybe, getTyVar_maybe)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Types.Name (Name, getName, getOccString, nameModule_maybe, nameSrcSpan)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Control.Monad (unless)
import qualified Cost.Translate.Output as O

-- Desugared types are used ONLY to validate instantiated class constraints.
-- Instrumentation still uses renamed source, never desugared/optimized Core.
-- Runtime Eq/Ord on a thunked list would compare thunk metadata, not source
-- elements. The approved primitive subset is restricted to strict scalars.
validateComparisons :: Core.CoreProgram -> Either String ()
validateComparisons = mapM_ binding
  where
    binding (Core.NonRec name expr) = walk (showSDocUnsafe (ppr (nameSrcSpan (getName name)))) expr
    binding (Core.Rec bindings) = mapM_ (binding . uncurry Core.NonRec) bindings
    walk location expr = do
        case expr of
            Core.Type _ -> pure ()
            Core.Coercion _ -> pure ()
            _ -> mapM_ (check location . scaledThing) (fst (splitFunTys (snd (splitForAllTyVars (exprType expr)))))
        case expr of
            Core.App f a -> walk location f >> walk location a
            Core.Lam _ body -> walk location body
            Core.Let bindings body -> binding bindings >> walk location body
            Core.Case scrutinee _ _ alts -> walk location scrutinee >> mapM_ (\(Core.Alt _ _ rhs) -> walk location rhs) alts
            Core.Cast body _ -> walk location body
            Core.Tick _ body -> walk location body
            _ -> pure ()
    check location ty = case splitTyConApp_maybe ty of
        Just (tc,[argument]) | getOccString tc `elem` ["Eq","Ord"], ghcName (getName tc) ->
            unless (scalar argument) (Left (location ++ ": comparisons support only strict scalar operands; found " ++ showSDocUnsafe (ppr argument)))
        _ -> pure ()

scalar :: Type -> Bool
scalar ty = isJust (getTyVar_maybe ty) || case splitTyConApp_maybe ty of
    Just (tc,[]) -> ghcName (getName tc) && getOccString tc `elem` ["Int","Integer","Bool","Float","Double","Char","()"]
    _ -> False

ghcName :: Name -> Bool
ghcName name = maybe False (("GHC." `isPrefixOf`) . moduleNameString . moduleName) (nameModule_maybe name)

-- Keep polymorphic exported primitive functions in the supported scalar domain,
-- even when a handwritten client instantiates them outside the source module.
scalarSupport :: [O.Decl]
scalarSupport = O.classD [] "Scalar" [O.b "a"] [] :
    [O.instanceD (O.tc "Scalar" [O.t ty]) [] | ty <- ["Int","Integer","Bool","Float","Double","Char","()"]]
