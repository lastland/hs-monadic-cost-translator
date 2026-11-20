module Cost.AcceptedSource
    ( AcceptedSource
    , acceptedSummary
    , verifyAccepted
    ) where

import Control.Monad (unless)
import GHC hiding (extensions)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.StringBuffer
import GHC.Driver.Session (xopt)
import GHC.LanguageExtensions.Type qualified as Extension
import System.Directory (canonicalizePath)

data AcceptedSource = AcceptedSource Module String [Bool] [Int]

acceptedSummary :: ModSummary -> IO (ModSummary, AcceptedSource)
acceptedSummary summary = do
    buffer <-
        maybe
            (hGetStringBuffer (ms_hspp_file summary))
            pure
            (ms_hspp_buf summary)
    let accepted =
            AcceptedSource
                (ms_mod summary)
                (lexemeToString buffer (len buffer))
                (extensions summary)
                (options summary)
    pure (summary {ms_hspp_buf = Just buffer}, accepted)

verifyAccepted :: [FilePath] -> AcceptedSource -> [ModSummary] -> IO [Module]
verifyAccepted paths (AcceptedSource owner snapshot exts opts) summaries = do
    matches <- mapM verify summaries
    pure [ms_mod summary | (summary, True) <- zip summaries matches]
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
                        let changed =
                                [ label
                                | (label, different) <-
                                    [ ("module/unit", ms_mod summary /= owner)
                                    , ("frontend buffer", contents /= snapshot)
                                    , ("extensions", extensions summary /= exts)
                                    , ("general flags", options summary /= opts)
                                    ]
                                , different
                                ]
                        unless
                            (null changed)
                            ( fail
                                ( "accepted origin snapshot mismatch: "
                                    ++ unwords changed
                                )
                            )
                        pure True

extensions :: ModSummary -> [Bool]
extensions summary =
    [ xopt flag (ms_hspp_opts summary)
    | flag <- [minBound .. maxBound :: Extension.Extension]
    ]

options :: ModSummary -> [Int]
options summary =
    [ fromEnum flag
    | flag <- EnumSet.toList (generalFlags (ms_hspp_opts summary))
    , flag /= Opt_IgnoreInterfacePragmas
    ]
