-- | Supplemental catalogue diagnostics, not a substitute for the public guard.
-- TH graphs are rejected at the public syntax seam before their methods run.
module ConstructionStockMetadata (checkTHStockMetadata) where

import ConstructionAst (allBindings)
import ConstructionDemandMethods (Stock(..), StockMethods(..), stockMethods)
import ConstructionRuntime (runtime, standardNames)
import ConstructionStockRequests (stockProvenance)
import Control.Monad ((>=>), unless, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds)
import GHC.Core.InstEnv (is_cls, is_dfun)
import GHC.Data.OrdList (fromOL)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.HsToCore.Binds (dsTopLHsBinds)
import GHC.HsToCore.Monad (initDs)
import GHC.Paths (libdir)
import GHC.Tc.Types (tcg_insts)
import System.FilePath (takeDirectory)

checkTHStockMetadata :: FilePath -> IO ()
checkTHStockMetadata path = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    (configured, leftovers, _) <- parseDynamicFlagsCmdLine flags (map noLoc
        ["-fno-ignore-interface-pragmas", "-package", "MemoristANT", "-package", "QuickCheck"])
    unless (null leftovers) (fail "cannot configure supplemental stock metadata")
    void $ setSessionDynFlags configured
        {backend = noBackend, ghcLink = NoLink, importPaths = [takeDirectory path]}
    -- Match the public guard's order: late runtime lookup after TH load panics
    -- in GHC 9.10.3's package-instance table.
    rt <- runtime
    target <- guessTarget path Nothing Nothing
    setTargets [target]
    graph <- depanal [] False
    Succeeded <- load LoadAllTargets
    checked <- mapM (parseModule >=> typecheckModule) (mgModSummaries graph)
    provenance <- mapM (stockProvenance rt) checked
    session <- getSession
    raw <- concat <$> mapM (\m -> do
        (_, Just bindings) <- liftIO $ initDs session (fst (tm_internals_ m)) $
            fromOL <$> dsTopLHsBinds (tm_typechecked_source m)
        pure bindings) checked
    let env = Map.fromList [(getName v, rhs) | (v, rhs) <- allBindings [C.Rec raw]]
        registered = [getName df | (m, names) <- zip checked provenance,
            StockMethods _ entries <- [stockMethods rt names m env (const [])],
            Stock df _ _ _ _ <- entries]
        eqDFuns = [getName (is_dfun inst) | m <- checked,
            inst <- tcg_insts (fst (tm_internals_ m)),
            Just selector <- [lookup "Prelude.==" (standardNames rt)],
            selector `elem` map getName (classAllSelIds (is_cls inst))]
    -- One actual specialized stock Eq and one actual generic manual Eq coexist.
    -- Neither may register; the real generic stock Show must still register.
    liftIO $ unless (length eqDFuns == 2 && length registered == 1 &&
        Set.null (Set.intersection (Set.fromList eqDFuns) (Set.fromList registered))) $
        fail (path ++ ": TH manual Eq borrowed stock provenance")
