-- | Explicit policy over the ReferencePolicy inventory. New sources succeed by
-- default; execution never modifies this catalog or promotes recovered cases.
module ReferenceSelection
    ( ReferenceCase(..), KnownFailure(..), referenceCases, caseKey
    , knownFailures, validateKnownFailures, knownFailure
    , ReferenceMode(..), selectReferences, listReference
    ) where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (isJust, isNothing)
import System.FilePath (takeDirectory, takeExtension)

data ReferenceCase = ReferenceCase
    { caseLabel :: String
    , caseSource :: FilePath
    } deriving (Eq, Show)

data KnownFailure = KnownFailure
    { failureKey :: String
    , failureReason :: String
    , failureIssue :: FilePath
    } deriving (Eq, Show)

referenceCases :: [(String, [FilePath])] -> [ReferenceCase]
referenceCases groups = [ReferenceCase label source | (label, sources) <- groups, source <- sources]

caseKey :: ReferenceCase -> String
caseKey item = caseLabel item ++ ":" ++ caseSource item

-- Reviewed source-level failures at e5d4652. Additions require fresh evidence
-- and explicit review, never the outcome of the current test run.
knownFailures :: [KnownFailure]
knownFailures =
    [KnownFailure "anf-scoped-renaming:test/cases/anf-scoped-renaming/Renamed.hs"
        "Repeated scoped factory calls spend 24/32/56 ticks instead of 17/18/35"
        ".scratch/reference-failures/issues/01-source-global-sharing.md"]

knownFailure :: ReferenceCase -> Maybe KnownFailure
knownFailure item = case filter ((== caseKey item) . failureKey) knownFailures of
    [record] -> Just record
    _ -> Nothing

data ReferenceMode = Success | KnownFailures | AllReferences deriving Eq

selectReferences :: ReferenceMode -> [String] -> [ReferenceCase] -> [ReferenceCase]
selectReferences mode selectors = filter selected
  where
    selected item = (null selectors || caseLabel item `elem` selectors || caseKey item `elem` selectors) &&
        case mode of
            Success -> isNothing (knownFailure item)
            KnownFailures -> isJust (knownFailure item)
            AllReferences -> True

listReference :: ReferenceCase -> String
listReference item = classification ++ "\t" ++ caseKey item ++ "\t" ++ reason ++
    "\tsh scripts/stack-test.sh cost-translate --test-arguments=" ++ quote (caseKey item)
  where
    (classification, reason) = case knownFailure item of
        Nothing -> ("success", "must pass")
        Just record -> ("known-failure", failureReason record ++ " (" ++ failureIssue record ++ ")")
    quote value = "'" ++ concatMap (\character ->
        if character == '\'' then "'\\''" else [character]) value ++ "'"

validateKnownFailures :: [ReferenceCase] -> [KnownFailure] -> IO ()
validateKnownFailures inventory records = do
    let keys = map caseKey inventory
        failures = map failureKey records
        problems =
            ["duplicate reference case key" | length keys /= length (nub keys)] ++
            ["duplicate known-failure key" | length failures /= length (nub failures)] ++
            ["unknown known-failure key: " ++ key | key <- failures, key `notElem` keys] ++
            ["known failure requires a reason and local issue: " ++ failureKey record
            | record <- records, all isSpace (failureReason record) ||
                takeDirectory (failureIssue record) /= ".scratch/reference-failures/issues" ||
                takeExtension (failureIssue record) /= ".md"]
    unless (null problems) (fail (unlines problems))
