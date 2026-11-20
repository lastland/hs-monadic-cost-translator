-- A bounded companion to the ordinary polymorphic-opaque-producer lifecycle.
-- Start from a fresh public translation. Check the two entry points and their
-- sharing dependencies before any caller is built. The ordinary reference runner
-- independently checks the entire publication, including all datatype support.
module ProducerSharing (checkProducerSharing) where

import ConstructionAst (collectAst, pretty)
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
import System.Directory (copyFile, createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)

checkProducerSharing :: IO ()
checkProducerSharing = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "opaque-producer-"
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
        bounded = root </> "sharing"
    putStrLn ("Opaque producer artifacts: " ++ root)
    fixture <- getDataFileName ("test/cases/polymorphic-opaque-producer" </> "Example.hs")
    copyFile fixture source
    translateFile source output >>= either fail pure
    checkTopLevelFunctions source output
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
    -- Preserve the original header/imports and exact text of selected declarations.
    -- A missing dependency is a compile failure, not an alternate handwritten body.
    text <- lines <$> readFile output
    let ranges = [(srcSpanStartLine s, srcSpanEndLine s) | d <- decls, keep (unLoc d),
                    RealSrcSpan s _ <- [getLocA d]]
        header = takeWhile (\(i,_) -> i < firstDecl) (zip [1..] text)
        firstDecl = minimum [srcSpanStartLine s | d <- decls, RealSrcSpan s _ <- [getLocA d]]
        excerpt = map snd header ++ [line | (i,line) <- zip [1..] text,
                                      any (\(lo,hi) -> lo <= i && i <= hi) ranges]
    createDirectory bounded
    copyFile source (bounded </> "Example.hs")
    writeFile (bounded </> "Generated.hs") (unlines excerpt)
    checkGeneratedConstruction (bounded </> "Generated.hs")
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
    putStrLn "PASS: opaque-producer-sharing / fresh sharing dependency graph only / shared worker, one root, no legacy calls"
