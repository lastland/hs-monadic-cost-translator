module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, forM_, unless)
import Data.Data (Data, cast, gmapQ)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, maybeToList)
import GHC
    ( FieldOcc(..), GhcRn, LIdP, GhcLink(NoLink), LoadHowMuch(LoadAllTargets), SuccessFlag(..)
    , backend, findModule, getLocA, getLogger, getModuleInfo
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, mkModuleName
    , load, ml_hs_file, modInfoExports, ms_location, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, tm_renamed_source, typecheckModule
    , unLoc, ghcLink
    )
import GHC.Paths (libdir)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.SrcLoc (noLoc)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>), takeDirectory)

-- Resolve actual runtime definitions, not an import's spelling or qualifier.
-- The renamed tree retains references in unused local/top-level helpers.
checkGeneratedConstruction :: FilePath -> IO ()
checkGeneratedConstruction output = do
    problems <- runGhc (Just libdir) $ do
        flags <- getSessionDynFlags
        logger <- getLogger
        (configured, leftovers, _) <- parseDynamicFlags logger flags
            (map noLoc ["-package", "MemoristANT", "-package", "QuickCheck"])
        unless (null leftovers) (fail "cannot configure generated-construction runtime packages")
        _ <- setSessionDynFlags configured
            { backend = noBackend, ghcLink = NoLink, importPaths = [takeDirectory output] }
        forbidden <- concat <$> forM reasoningOperations (\(definingModule, occurrences) -> do
            actualModule <- findModule (mkModuleName definingModule) Nothing
            info <- getModuleInfo actualModule >>= maybe
                (fail ("cannot inspect reasoning operations in " ++ definingModule)) pure
            let definedHere = [name | name <- modInfoExports info, nameModule_maybe name == Just actualModule]
                required = fromMaybe (map getOccString definedHere) occurrences
            forM required $ \occurrence -> case
                    [name | name <- definedHere, getOccString name == occurrence] of
                [name] -> pure (name, definingModule ++ "." ++ occurrence)
                _ -> fail ("cannot resolve reasoning operation " ++ definingModule ++ "." ++ occurrence))
        target <- guessTarget output Nothing Nothing
        setTargets [target]
        graph <- depanal [] False
        -- Load home imports as GHC normally would (e.g. source conversion types
        -- or a re-export), but inspect only this generated artifact's tree.
        loaded <- load LoadAllTargets
        case loaded of
            Failed -> fail (output ++ ": cannot load generated module for name resolution")
            Succeeded -> pure ()
        checked <- case [summary | summary <- mgModSummaries graph,
                ml_hs_file (ms_location summary) == Just output] of
            [summary] -> parseModule summary >>= typecheckModule
            _ -> fail (output ++ ": expected one generated module")
        case tm_renamed_source checked of
            Just (group, _, _, _, _) -> pure
                [ showSDocUnsafe (ppr location) ++ ": reasoning-only operation " ++
                  operation ++ " is forbidden in generated code (" ++ output ++ ")"
                | (location, name) <-
                    [(getLocA reference, unLoc reference) | reference <- collectAst group :: [LIdP GhcRn]] ++
                    [(getLocA label, name) | FieldOcc name label <- collectAst group :: [FieldOcc GhcRn]]
                , Just operation <- [lookup name forbidden]
                ]
            Nothing -> fail (output ++ ": GHC did not retain renamed source")
    unless (null problems) (fail (unlines problems))

-- Nothing selects the whole cost-reasoning module, including its property APIs.
-- Thunk operations and legitimate demand-conversion APIs remain legal.
reasoningOperations :: [(String, Maybe [String])]
reasoningOperations =
    [("Memorist.Base.Core", Just
        ["eval", "eval1", "value", "annot", "costAnnot", "usageAnnot", "runM", "runMMBT", "runMBT", "runAnnotT"])
    ,("Memorist.Base.ANT", Just ["runMT"])
    ,("Memorist.Infer.Infer", Nothing)]

collectAst :: (Data a, Data b) => b -> [a]
collectAst value = maybeToList (cast value) ++ concat (gmapQ collectAst value)

checkGeneratedConstructionSpecimens :: IO ()
checkGeneratedConstructionSpecimens = do
    rejects "QualifiedAlias.hs" [(4, "Memorist.Base.Core.value")]
    rejects "Unqualified.hs" [(7, "Memorist.Base.Core.eval1")]
    rejects "LetWrapper.hs" [(6, "Memorist.Base.Core.value")]
    rejects "UnusedHelper.hs" [(6, "Memorist.Base.Core.eval1")]
    rejects "Reexport.hs" [(6, "Memorist.Base.Core.value")]
    rejects "BoundaryReferences.hs"
        [(7, "Memorist.Base.Core.eval"), (10, "Memorist.Base.Core.eval1")
        ,(13, "Memorist.Base.Core.value"), (16, "Memorist.Base.Core.runM")
        ,(19, "Memorist.Base.Core.runMMBT"), (20, "Memorist.Base.Core.runMBT")
        ,(21, "Memorist.Base.Core.runAnnotT"), (22, "Memorist.Base.ANT.runMT")]
    rejects "ReasoningPublication.hs" [(8, "Memorist.Base.Core.value"), (8, "Memorist.Base.Core.eval1")]
    rejects "ReasoningAPIs.hs"
        [(6, "Memorist.Infer.Infer.infcost"), (9, "Memorist.Infer.Infer.infcost'")
        ,(12, "Memorist.Infer.Infer.calcCost"), (15, "Memorist.Infer.Infer.correct")
        ,(18, "Memorist.Base.Core.annot"), (21, "Memorist.Base.Core.costAnnot")
        ,(24, "Memorist.Base.Core.usageAnnot")]
    mapM_ accepts ["ImportAliasControl.hs", "LocalShadowing.hs", "Construction.hs"]
    putStrLn "PASS: generated-construction / resolved evaluation, extraction and cost-reasoning references rejected through aliases, re-exports and unused wrappers / user names, lazy/force, tval, comments and strings accepted"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    accepts name = specimen name >>= checkGeneratedConstruction
    rejects name expected = do
        output <- specimen name
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem -> forM_ expected $ \(line, operation) ->
                unless (any (\diagnostic -> all (`isInfixOf` diagnostic)
                        [output ++ ":" ++ show (line :: Int) ++ ":", "reasoning-only operation " ++ operation,
                         "is forbidden in generated code"]) (lines (displayException problem))) $
                    fail (output ++ ": expected forbidden " ++ operation ++ ", got " ++ displayException problem)
            Right () -> fail (output ++ ": expected forbidden reasoning operation, got Right ()")
