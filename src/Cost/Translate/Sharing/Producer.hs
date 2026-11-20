module Cost.Translate.Sharing.Producer (planProducers, emitProducer) where

import Control.Monad (guard)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Frontend.Allocation (freshPrimed)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expressions (tick)
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Polymorphic
import Cost.Translate.Frontend.Signatures (monadicType)
import Cost.Translate.Frontend.Syntax (collect, plainRhs)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing.Native (nativeSharing)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import GHC
import GHC.Core.TyCo.FVs (tyCoVarsOfTypes)
import GHC.Core.Type
    ( isLiftedTypeKind
    , isManyTy
    , mkSpecForAllTy
    , splitFunTy_maybe
    , tyVarKind
    )
import GHC.Core.Unify (tcMatchTy)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Basic (RecFlag (..))
import GHC.Types.Name (getOccString)
import GHC.Types.Var (FunTyFlag (FTF_T_T))
import GHC.Types.Var.Set (elemVarSet)

data Producer = Producer String Name [TyVar] TyVar Type Payload

planProducers ::
    Env ->
    [(RecFlag, [LHsBind GhcRn])] ->
    (Map.Map Name Producer, Map.Map Name String)
planProducers env groups =
    (Map.fromList workers, Map.fromList (concat sites))
    where
        requests = Map.fromListWith (++) (concatMap localCalls bindings)
        bindings = concatMap snd groups
        localCalls binding =
            [ (unLoc callee, [(name, ty)])
            | L _ (FunBind _ n matches) <- collect binding :: [LHsBind GhcRn]
            , let name = unLoc n
            , not (isExternalName name)
            , [L _ match] <- [unLoc (mg_alts matches)]
            , null (m_pats match)
            , Just (L _ (HsApp _ (L _ (HsVar _ callee)) _)) <- [plainRhs match]
            , Just ty <- [Map.lookup name (inferredBindings env)]
            , ([variable], [], _) <- [tcSplitSigmaTy ty]
            , isLiftedTypeKind (tyVarKind variable)
            , Just [getName variable] == Map.lookup name scopes
            , Right Nothing <- [nativeSharing schemas Map.empty ty]
            ]
        schemas = plannedNativeSchemas (namePlan env)
        scopes = scopedTypeVariables env
        (workers, sites) =
            unzip
                [ ((owner, worker), uses)
                | (NonRecursive, group) <- groups
                , L _ (FunBind _ name matches) <- group
                , let owner = unLoc name
                , Just calls <- [Map.lookup owner requests]
                , [L _ match] <- [unLoc (mg_alts matches)]
                , Just (worker, uses) <- [planWorker env owner match calls]
                ]

planWorker ::
    Env ->
    Name ->
    Match GhcRn (LHsExpr GhcRn) ->
    [(Name, Type)] ->
    Maybe (Producer, [(Name, String)])
planWorker env owner match calls = do
    [L _ (VarPat _ formal)] <- pure (m_pats match)
    expression <- plainRhs match
    ty <- Map.lookup owner (inferredBindings env)
    let (variables, predicates, body) = tcSplitSigmaTy ty
    guard (null predicates && owner `Set.notMember` reflectiveBindings env)
    (FTF_T_T, mult, argument, result) <- splitFunTy_maybe body
    guard (isManyTy mult)
    target <- Map.lookup owner (plannedBodies (namePlan env))
    [(outer, inner, uses)] <-
        pure
            [ (outer, inner, uses)
            | inner <- variables
            , let outer = filter (/= inner) variables
                  captures = tyCoVarsOfTypes (argument : map tyVarKind outer)
            , not (inner `elemVarSet` captures)
            , let scheme = mkSpecForAllTy inner result
                  matches = isJust . tcMatchTy scheme
                  sites = [site | (site, local) <- calls, matches local]
                  uses = map (,target) sites
            , not (null uses)
            ]
    payload <- producerPayload env (unLoc formal) inner result expression
    pure (Producer target (unLoc formal) outer inner argument payload, uses)

emitProducer ::
    (Env -> LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    String ->
    O.Ty ->
    Producer ->
    Either String [O.Decl]
emitProducer emit env public signature producer = do
    let Producer worker formal outer inner argument payload = producer
        datatypes = sourceDatatypes env
        parameter = readableBinder env formal
        scope = insertEnv formal (Thunk (O.v parameter)) env
        value = freshPrimed (Set.fromList [parameter, worker]) "payload"
        atVariable function = O.typeApp function . O.t . getOccString
        call = O.app (foldl atVariable (O.v worker) outer) [O.v parameter]
        instantiate = O.ret (atVariable (O.v value) inner)
        wrapper = O.doE [O.bind value call] instantiate
    binders <- mapM (renderBinder datatypes) outer
    input <- renderType datatypes argument
    result <- payloadType env payload
    action <- emitPayload (emit scope) scope payload
    let bodyType = O.arrow (O.tc "M.Th" [input]) (monadicType result)
        workerType = O.forallT binders bodyType
    pure
        [ O.sig worker workerType
        , O.val worker [O.p parameter] (O.doE [O.thenS tick] action)
        , O.sig public signature
        , O.val public [O.p parameter] wrapper
        ]
