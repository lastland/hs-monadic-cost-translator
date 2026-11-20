-- A bounded companion to the ordinary polymorphic-opaque-producer lifecycle.
-- Start from a fresh public translation. Check the two entry points and their
-- sharing dependencies before any caller is built. The ordinary reference runner
-- independently checks the entire publication, including all datatype support.
module ProducerSharing (checkProducerSharing) where

import ConstructionAst (collectAst, pretty)
import Control.Monad (forM_, unless)
import Cost.Translate (translateFile)
import Data.List (intersect)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GeneratedConstruction (checkGeneratedConstruction)
import GeneratedFunctions (checkTopLevelFunctions, parseHaskellModule)
import GHC
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)
import Paths_cost_translate (getDataFileName)
import ReferenceRunner (ReferenceEnvironment, buildObserver, comparer)
import System.Directory (copyFile, createDirectory, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)

checkProducerSharing :: ReferenceEnvironment -> IO ()
checkProducerSharing environment = forM_ ["Example.hs", "HelperCollision.hs"] $ \fixture -> do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "opaque-producer-"
    fixtures <- getDataFileName "test/cases/polymorphic-opaque-producer"
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
        bounded = root </> "sharing"
        reference = root </> "reference"
    putStrLn ("Opaque producer artifacts: " ++ root)
    copyFile (fixtures </> fixture) source
    translateFile source output >>= either fail pure
    checkTopLevelFunctions source output
    createDirectory bounded
    checkSharingGraph output (bounded </> "Generated.hs")
    createDirectory reference
    forM_ [bounded, reference] $ \sandbox -> do
        copyFile source (sandbox </> "Example.hs")
        copyFile (fixtures </> "Case.hs") (sandbox </> "Case.hs")
    copyFile (fixtures </> "Reference.hs") (reference </> "Generated.hs")
    actual <- buildObserver environment bounded
    expected <- buildObserver environment reference
    (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
    writeFile (root </> "comparison.log") (out ++ err)
    unless (status == ExitSuccess) (fail (out ++ err))
    putStrLn ("PASS: " ++ fixture ++ " / fresh sharing dependency graph only / unchanged caller and independent reference")

checkSharingGraph :: FilePath -> FilePath -> IO ()
checkSharingGraph output bounded = do
    ast <- parseHaskellModule output
    let decls = hsmodDecls ast
        refs b = [pretty (unLoc n) | HsVar _ n <- collectAst b :: [HsExpr GhcPs]]
        calls = Map.fromList [(pretty (unLoc n), refs b) | L _ (ValD _ b@(FunBind _ n _)) <- decls]
        visit done [] = done
        visit done (name:rest)
            | Set.member name done = visit done rest
            | otherwise = visit (Set.insert name done)
                (filter (`Map.member` calls) (Map.findWithDefault [] name calls) ++ rest)
        reachable = visit Set.empty ["makeM", "viewsM"]
        keep :: HsDecl GhcPs -> Bool
        keep (ValD _ (FunBind _ n _)) = pretty (unLoc n) `Set.member` reachable
        keep (SigD _ (TypeSig _ ns _)) = any ((`Set.member` reachable) . pretty . unLoc) ns
        keep (TyClD _ d@DataDecl{}) = occNameString (rdrNameOcc (tcdName d)) == "HigherT"
        keep _ = False
    -- Use the original pragmas, required imports and exact text of the declarations.
    -- A missing dependency is a compile failure, not an alternate handwritten body.
    text <- lines <$> readFile output
    let ranges = [(srcSpanStartLine s, srcSpanEndLine s) | d <- decls, keep (unLoc d),
                    RealSrcSpan s _ <- [getLocA d]]
        header = take (firstImport - 1) text
        firstImport = minimum [srcSpanStartLine s | i <- hsmodImports ast, RealSrcSpan s _ <- [getLocA i]]
        imports = [(srcSpanStartLine s, srcSpanEndLine s) | i <- hsmodImports ast,
                     moduleNameString (unLoc (ideclName (unLoc i))) `elem` ["Data.Kind", "Prelude", "Memorist.Memorist"],
                     RealSrcSpan s _ <- [getLocA i]]
        excerpt = header ++ [line | (i,line) <- zip [1..] text,
                             any (\(lo,hi) -> lo <= i && i <= hi) (imports ++ ranges)]
    writeFile bounded (unlines excerpt)
    checkGeneratedConstruction bounded
    let called name = Map.findWithDefault [] name calls
        common = filter (`Map.member` calls) (called "makeM" `intersect` called "viewsM")
        count x = length . filter (== x)
        shared = called "viewsM"
        legacy = ["sharedAt", "sharedAtBy", "M.eval", "M.value", "M.tname"]
    case common of
        [worker] | count "M.lazy" shared == 1, count "mapThunk" shared == 1,
                   count "M.PairT" shared == 1, count "M.tick" shared == 1,
                   count "M.tick" (called worker) == 1, count "HigherT" (called worker) == 1,
                   count "M.tick" (called "makeM") == 0,
                   null (legacy `intersect` concatMap called (Set.toList reachable)) -> pure ()
        _ -> fail "expected one shared producer body/entry tick and one lazy root with type-instantiation views"
