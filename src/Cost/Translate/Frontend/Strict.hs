module Cost.Translate.Frontend.Strict
    ( strictWhnfBindings
    , strictBindNames
    ) where

import Cost.Translate.Frontend.Syntax (collect)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import GHC
import GHC.Core qualified as Core
import GHC.Core.Utils (exprIsHNF)
import GHC.Data.Bag (bagToList, unitBag)
import GHC.HsToCore.Binds (dsLHsBinds)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types.Evidence (HsWrapper (..))
import GHC.Types.Var (isNonCoVarId)

strictWhnfBindings :: HscEnv -> TypecheckedModule -> IO (Set.Set Name)
strictWhnfBindings session checked = do
    (_, result) <-
        initDs session (fst (tm_internals_ checked)) $
            Map.traverseWithKey prove candidates
    pure (maybe Set.empty (Map.keysSet . Map.filter id) result)
    where
        strict =
            Set.fromList
                [ name
                | (group, _, _, _, _) <- maybeToList (tm_renamed_source checked)
                , name <- strictBindNames (collect group)
                ]
        candidates =
            Map.fromListWith
                (++)
                [ (getName (abe_poly export), [binding])
                | binding@(L _ (XHsBindsLR abstraction)) <-
                    collect (tm_typechecked_source checked) :: [LHsBind GhcTc]
                , export <- abs_exports abstraction
                , getName (abe_poly export) `Set.member` strict
                ]
        prove name [binding] | Just evidence <- strictEvidence name binding = do
            (forced, pairs) <- dsLHsBinds (unitBag binding)
            pure $ case [ (identifier, rhs)
                        | (identifier, rhs) <- pairs
                        , getName identifier == name
                        ] of
                [(_, rhs)] ->
                    name `elem` map getName forced
                        && exprIsHNF rhs
                        && any
                            (runtimeEvidence evidence)
                            (fst (Core.collectBinders rhs))
                _ -> False
        prove _ _ = pure False

runtimeEvidence :: [Id] -> Core.CoreBndr -> Bool
runtimeEvidence evidence binder =
    isNonCoVarId binder && binder `elem` evidence

strictEvidence :: Name -> LHsBind GhcTc -> Maybe [Id]
strictEvidence name (L _ (XHsBindsLR abstraction)) =
    case [ export
         | export <- abs_exports abstraction
         , getName (abe_poly export) == name
         ] of
        [export] -> case [ wrapper
                         | L _ (FunBind (wrapper, _) identifier _) <-
                            bagToList (abs_binds abstraction)
                         , unLoc identifier == abe_mono export
                         ] of
            [wrapper] -> do
                exported <- evidenceBinders (abe_wrap export)
                own <- evidenceBinders wrapper
                pure (abs_ev_vars abstraction ++ exported ++ own)
            _ -> Nothing
        _ -> Nothing
strictEvidence _ _ = Nothing

evidenceBinders :: HsWrapper -> Maybe [Id]
evidenceBinders WpHole = Just []
evidenceBinders (WpCompose left right) =
    (++) <$> evidenceBinders left <*> evidenceBinders right
evidenceBinders (WpEvLam identifier) = Just [identifier]
evidenceBinders (WpTyLam _) = Just []
evidenceBinders (WpLet _) = Just []
evidenceBinders _ = Nothing

strictBindNames :: [LHsBind GhcRn] -> [Name]
strictBindNames bindings =
    [ unLoc name
    | L _ (FunBind _ name matches) <- bindings
    , L _ Match {m_ctxt = FunRhs {mc_strictness = SrcStrict}} <-
        unLoc (mg_alts matches)
    ]
