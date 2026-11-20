module Cost.AcceptedSource
    ( AcceptedSource
    , acceptedSummary
    , verifyAccepted
    , acceptedSyntax
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Cost.AcceptedOptions
import GHC
import GHC.Data.StringBuffer
import System.Directory (canonicalizePath)

data AcceptedSource
    = AcceptedSource Module String AcceptedOptions (HsModule GhcPs)

acceptedSyntax :: AcceptedSource -> HsModule GhcPs
acceptedSyntax (AcceptedSource _ _ _ syntax) = syntax

acceptedSummary :: HscEnv -> ModSummary -> Ghc (ParsedModule, AcceptedSource)
acceptedSummary session summary = do
    buffer <-
        liftIO $
            maybe
                (hGetStringBuffer (ms_hspp_file summary))
                pure
                (ms_hspp_buf summary)
    options <- liftIO (acceptedOptions session summary)
    parsed <- parseModule summary {ms_hspp_buf = Just buffer}
    let accepted =
            AcceptedSource
                (ms_mod summary)
                (lexemeToString buffer (len buffer))
                options
                (unLoc (pm_parsed_source parsed))
    pure (parsed, accepted)

verifyAccepted ::
    HscEnv -> [FilePath] -> AcceptedSource -> [ModSummary] -> IO [Module]
verifyAccepted session paths (AcceptedSource owner snapshot opts _) modules = do
    matches <- mapM verify modules
    pure [ms_mod summary | (summary, True) <- zip modules matches]
    where
        verify summary = case ml_hs_file (ms_location summary) of
            Nothing -> pure False
            Just file -> do
                path <- canonicalizePath file
                if path `notElem` paths
                    then do
                        unless
                            (ms_mod summary /= owner)
                            (fail "origin loaded from an unrecorded path")
                        pure False
                    else do
                        buffer <-
                            maybe
                                (hGetStringBuffer (ms_hspp_file summary))
                                pure
                                (ms_hspp_buf summary)
                        let contents = lexemeToString buffer (len buffer)
                        actualOptions <- acceptedOptions session summary
                        let changed =
                                [ label
                                | (label, different) <-
                                    [ ("module/unit", ms_mod summary /= owner)
                                    , ("frontend buffer", contents /= snapshot)
                                    ]
                                , different
                                ]
                                    ++ changedOptions opts actualOptions
                        unless
                            (null changed)
                            ( fail
                                ( "accepted origin snapshot mismatch: "
                                    ++ unwords changed
                                )
                            )
                        pure True
