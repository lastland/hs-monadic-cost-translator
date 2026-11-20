-- Private attestation controls, not a second source of publication authority.
-- Every record below comes from real staging, translation and publication.
module AcceptedOptionsChecks (checkAcceptedOptions) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (liftIO)
import Cost.Publication (stageAndTranslate, verifyOrigin)
import Data.List (isInfixOf)
import GHC
import GHC.Data.StringBuffer (hGetStringBuffer)
import GHC.Driver.Session (Language(Haskell98), parseDynamicFlagsCmdLine, gopt_set, xopt, xopt_set, xopt_unset)
import GHC.LanguageExtensions.Type qualified as Extension
import GHC.Paths (libdir)
import GHC.Types.Basic (treatZeroAsInf)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.IO.Temp (withSystemTempDirectory)

checkAcceptedOptions :: IO ()
checkAcceptedOptions = withSystemTempDirectory "accepted-options-" $ \root -> do
    fixture <- getDataFileName "test/cases/source-map/Example.hs"
    let output = root </> "Generated.hs"
        source = root </> "Example.hs"
    publication <- stageAndTranslate fixture output >>= either fail pure
    runGhc (Just libdir) $ do
        initial <- getSessionDynFlags
        (flags, leftovers, _) <- parseDynamicFlagsCmdLine initial
            (map noLoc ["-fno-ignore-interface-pragmas", "-package", "MemoristANT",
                "-package", "QuickCheck", "-package", "ghc-prim"])
        unless (null leftovers) (fail "cannot configure private option controls")
        setSessionDynFlags flags { backend = noBackend, ghcLink = NoLink, importPaths = [root] }
        target <- guessTarget output Nothing Nothing
        setTargets [target]
        Succeeded <- load LoadAllTargets
        summaries <- mgModSummaries <$> getModuleGraph
        pinned <- forM summaries $ \summary -> do
            buffer <- liftIO $ maybe (hGetStringBuffer (ms_hspp_file summary)) pure (ms_hspp_buf summary)
            pure summary { ms_hspp_buf = Just buffer }
        unless (length [() | summary <- pinned, ml_hs_file (ms_location summary) == Just source] == 1)
            (fail "private controls require exactly the real staged origin")
        let change update summary
                | ml_hs_file (ms_location summary) == Just source =
                    summary { ms_hspp_opts = update (ms_hspp_opts summary) }
                | otherwise = summary
            verify = verifyOrigin publication output
        liftIO $ void (verify pinned)
        -- This is the one intentional inspection difference, not a blanket
        -- exemption for visibility flags or arbitrary compiler settings.
        liftIO $ void (verify (map (change (\f -> gopt_set f Opt_IgnoreInterfacePragmas)) pinned))
        rejected <- forM optionChanges $ \(label, update) -> liftIO $ do
            result <- try (verify (map (change update) pinned))
            pure (label, case result of
                Left (err :: IOException) -> "accepted origin snapshot mismatch" `isInfixOf` ioeGetErrorString err
                Right _ -> False)
        unless (all snd rejected) $
            fail ("unchanged accepted buffers acquired authority after changed options: " ++
                unwords [label | (label, False) <- rejected])
    putStrLn "PASS: accepted options / genuine publication, identical buffers, changed effective settings rejected"

optionChanges :: [(String, DynFlags -> DynFlags)]
optionChanges =
    [ ("safeHaskell", \f -> f { safeHaskell = Sf_Safe })
    , ("reductionDepth", \f -> f { reductionDepth = treatZeroAsInf 37 })
    , ("solverIterations", \f -> f { solverIterations = treatZeroAsInf 37 })
    , ("givensFuel", \f -> f { givensFuel = givensFuel f + 1 })
    , ("wantedsFuel", \f -> f { wantedsFuel = wantedsFuel f + 1 })
    , ("qcsFuel", \f -> f { qcsFuel = qcsFuel f + 1 })
    , ("language", \f -> f { language = Just Haskell98 })
    , ("visibility", \f -> gopt_set f Opt_HideAllPackages)
    , ("kind checking", \f -> if xopt Extension.PolyKinds f
          then xopt_unset f Extension.PolyKinds else xopt_set f Extension.PolyKinds)
    ]
