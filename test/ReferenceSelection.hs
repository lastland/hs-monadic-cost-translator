-- | Explicit policy over the ReferencePolicy inventory. New sources succeed by
-- default; execution never modifies this catalog or promotes recovered cases.
module ReferenceSelection
    ( ReferenceCase(..), KnownFailure(..), referenceCases, caseKey
    , knownFailures, validateKnownFailures, knownFailure
    , ReferenceMode(..), selectReferences, listReference
    ) where

import Control.Monad (filterM, unless)
import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (isJust, isNothing)
import System.FilePath (takeDirectory, takeExtension)
import System.Directory (doesFileExist)
import Paths_cost_translate (getDataFileName)

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

-- Reviewed initial source pin:
-- e5d4652cd08a4cb4a02d1b9fbc9ac22d67026268
-- Independent audit report SHA256:
-- 575086a9ad82d2aa60d3596aa4fe8c35a5d6c125fd41ac0b4c8c3a9473f17b96
-- See .scratch/reference-failures/map.md for the reviewed source/issue mapping.
-- Additions require explicit review, never the outcome of the current run.
knownFailures :: [KnownFailure]
knownFailures =
    [KnownFailure "anf-scoped-renaming:test/cases/anf-scoped-renaming/Renamed.hs"
        "Repeated scoped factory calls spend 24/32/56 ticks instead of 17/18/35"
        ".scratch/reference-failures/issues/01-source-global-sharing.md"
    ,KnownFailure "anf-scoped-renaming:test/cases/anf-scoped-renaming/Shadowed.hs"
        "Repeated scoped factory calls spend 24/32/56 ticks instead of 17/18/35"
        ".scratch/reference-failures/issues/01-source-global-sharing.md"
    ,KnownFailure "constrained-sharing-all-dictionaries:test/cases/constrained-sharing-all-dictionaries/Example.hs"
        "Interleaved dictionary keys cost 16 ticks instead of 10"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-constructor-dictionaries:test/cases/constrained-sharing-constructor-dictionaries/Example.hs"
        "Interleaved constructor dictionaries cost 16 ticks instead of 10"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-different-dictionaries:test/cases/constrained-sharing-different-dictionaries/Example.hs"
        "Interleaved dictionaries cost 20 ticks instead of 11"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-generic-caller:test/cases/constrained-sharing-generic-caller/Example.hs"
        "Generic interleaved dictionary calls cost 28 ticks instead of 19"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-nested-allocations:test/cases/constrained-sharing-nested-allocations/Example.hs"
        "Nested interleaved dictionary calls cost 28 ticks instead of 13"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-same-dictionary:test/cases/constrained-sharing-different-dictionaries/Example.hs"
        "Repeated same-dictionary calls cost 8 ticks instead of 5"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "constrained-sharing-unconstrained-parameter:test/cases/constrained-sharing-unconstrained-parameter/Example.hs"
        "Calls with unconstrained element types cost 16 ticks instead of 9"
        ".scratch/reference-failures/issues/02-constrained-sharing-costs.md"
    ,KnownFailure "datatypes-and-demands:test/cases/datatypes-and-demands/Example.hs"
        "Construction guard rejects nested reprM closure and demand-data Eq/Show"
        ".scratch/reference-failures/issues/06-demand-data-construction.md"
    ,KnownFailure "direct-concatMap-maybe:test/cases/direct-concatMap-maybe/Example.hs"
        "Construction guard rejects concatMapT with open FoldableT evidence"
        ".scratch/reference-failures/issues/07-open-foldable-construction.md"
    ,KnownFailure "direct-foldl-pair-unused-first-field:test/cases/direct-foldl-pair-unused-first-field/Example.hs"
        "Construction guard rejects foldlT with open FoldableT evidence"
        ".scratch/reference-failures/issues/07-open-foldable-construction.md"
    ,KnownFailure "direct-foldr-maybe:test/cases/direct-foldr-maybe/Example.hs"
        "Construction guard rejects foldrT with open FoldableT evidence"
        ".scratch/reference-failures/issues/07-open-foldable-construction.md"
    ,KnownFailure "direct-foldr-nothing-unused-callback:test/cases/direct-foldr-nothing-unused-callback/Example.hs"
        "Construction guard rejects foldrT with open FoldableT evidence"
        ".scratch/reference-failures/issues/07-open-foldable-construction.md"
    ,KnownFailure "generated-properties:test/cases/generated-properties/Example.hs"
        "Construction guard rejects foldrT with open FoldableT evidence"
        ".scratch/reference-failures/issues/07-open-foldable-construction.md"
    ,KnownFailure "manual-scoped-annotations:examples/ScopedAnnotations.hs"
        "Repeated scoped factory calls cost 12/16 ticks instead of 9/10"
        ".scratch/reference-failures/issues/03-scoped-factory-costs.md"
    ,KnownFailure "manual-thunk-matching:examples/ThunkMatching.hs"
        "Thunk identity mismatch; repeated dictionary calls cost 48 ticks instead of 26"
        ".scratch/reference-failures/issues/05-thunk-sharing-identities.md"
    ,KnownFailure "nested-pair-and-maybe-fields:test/cases/nested-pair-and-maybe-fields/Example.hs"
        "Construction guard rejects nested reprM closure and demand-data Eq/Show"
        ".scratch/reference-failures/issues/06-demand-data-construction.md"
    ,KnownFailure "polymorphic-shared-top-level-value:test/cases/polymorphic-shared-top-level-value/Example.hs"
        "Cross-type shared calls cost 16 ticks instead of 9"
        ".scratch/reference-failures/issues/04-top-level-sharing-costs.md"
    ,KnownFailure "scoped-factory-case-shadow-int:test/cases/scoped-factory-case-shadow/Example.hs"
        "Repeated scoped factory specialization costs 12 ticks instead of 9"
        ".scratch/reference-failures/issues/03-scoped-factory-costs.md"
    ,KnownFailure "scoped-factory-case-shadow-integer:test/cases/scoped-factory-case-shadow/Example.hs"
        "Repeated scoped factory specialization costs 16 ticks instead of 10"
        ".scratch/reference-failures/issues/03-scoped-factory-costs.md"
    ,KnownFailure "scoped-factory-guard-shadow-int:test/cases/scoped-factory-guard-shadow/Example.hs"
        "Repeated scoped factory specialization costs 12 ticks instead of 9"
        ".scratch/reference-failures/issues/03-scoped-factory-costs.md"
    ,KnownFailure "scoped-factory-guard-shadow-integer:test/cases/scoped-factory-guard-shadow/Example.hs"
        "Repeated scoped factory specialization costs 16 ticks instead of 10"
        ".scratch/reference-failures/issues/03-scoped-factory-costs.md"
    ,KnownFailure "shared-top-level-across-exported-calls:test/cases/shared-top-level-across-exported-calls/Example.hs"
        "Interleaved exported calls cost 16 ticks instead of 9"
        ".scratch/reference-failures/issues/04-top-level-sharing-costs.md"
    ,KnownFailure "thunk-matching-sharing:test/cases/thunk-matching-sharing/Example.hs"
        "Thunk identity mismatch; repeated polymorphic calls cost 24 ticks instead of 9"
        ".scratch/reference-failures/issues/05-thunk-sharing-identities.md"
    ,KnownFailure "thunk-sharing:test/cases/thunk-sharing/Example.hs"
        "Repeated demands of a shared thunk cost 6 ticks instead of 4"
        ".scratch/reference-failures/issues/05-thunk-sharing-identities.md"
    ,KnownFailure "top-level-function-sharing:test/cases/top-level-function-sharing/Example.hs"
        "Uncached top-level calls cost 10 ticks instead of 7"
        ".scratch/reference-failures/issues/04-top-level-sharing-costs.md"]

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
    missing <- filterM (\record -> not <$> (getDataFileName (failureIssue record) >>= doesFileExist))
        (filter localIssue records)
    let keys = map caseKey inventory
        failures = map failureKey records
        problems =
            ["duplicate reference case key" | length keys /= length (nub keys)] ++
            ["duplicate known-failure key" | length failures /= length (nub failures)] ++
            ["unknown known-failure key: " ++ key | key <- failures, key `notElem` keys] ++
            ["known failure requires a reason and local issue: " ++ failureKey record
            | record <- records, all isSpace (failureReason record) ||
                not (localIssue record)] ++
            ["missing known-failure issue target: " ++ failureIssue record | record <- missing]
    unless (null problems) (fail (unlines problems))
  where
    localIssue record =
        takeDirectory (failureIssue record) == ".scratch/reference-failures/issues" &&
        takeExtension (failureIssue record) == ".md"
