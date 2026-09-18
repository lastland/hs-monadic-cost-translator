-- A representative compiled builder probe. Its output is constructed solely
-- from GHC syntax, pretty-printed once, compiled and executed by GHC.
module Main (main) where

import qualified Cost.Translate.Output as O
import Cost.Translate.Pretty (renderModule)
import GHC (runGhc, getSessionDynFlags, HsModule, GhcPs)
import GHC.Paths (libdir)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (unless)

main :: IO ()
main = withSystemTempDirectory "output-ast-probe-" $ \root -> do
    flags <- runGhc (Just libdir) getSessionDynFlags
    let file = root </> "Main.hs"
    let code = renderModule flags ast
    writeFile file code
    (built,_,err) <- readProcessWithExitCode "ghc" ["-v0","-package","MemoristANT",file,"-o",root </> "probe"] ""
    unless (built == ExitSuccess) (fail (file ++ ": " ++ err))
    (ran,out,errors) <- readProcessWithExitCode (root </> "probe") [] ""
    unless (ran == ExitSuccess && out == "(True,1)\n") (fail (show (ran,out,errors)))
    putStrLn "PASS: GHC AST builder / declarations, kinds, records, statements, annotations / value and cost"

ast :: HsModule GhcPs
ast = O.moduleD "Main"
    [O.importD "Prelude" Nothing, O.importD "Data.Kind" Nothing,
     O.importD "Memorist.Memorist" (Just "M"), O.importD "Memorist.Base.Thunk" (Just "MT")]
    [ O.dataD "Box" [O.b "a"] [("Box", [O.t "a"])]
    , O.dataD "Plain" [O.b "a"] [("Plain", [a])]
    , O.dataD "Wrapped" [O.b "a"] [("Wrapped", [O.tc "M.Th" [a]])]
    , O.instanceD (O.forallT [O.b "a"] (O.qualified [O.tc "M.Repr" [a]] (O.tc "M.Repr" [O.tc "Plain" [a]])))
        [O.assoc "Rep" [O.tc "Plain" [a]] (O.tc "Wrapped" [O.tc "M.Rep" [a]]),
         O.val "reprM" [O.conP "Plain" [O.p "x"]] (O.doE [O.bind "tx" (O.call "M.lazy" [O.call "M.reprM" [O.v "x"]])] (O.ret (O.call "Wrapped" [O.v "tx"]))),
         O.val "abstract" [O.conP "Wrapped" [O.p "tx"]] (O.call "Plain" [O.call "M.abstract" [O.call "M.tval" [O.v "tx"]]])]
    , O.derivingD (O.forallT [O.b "a"] (O.qualified [O.tc "Show" [a]] (O.tc "Show" [O.tc "Box" [a]])))
    , O.recordD "Record" [("callback", O.forallT [O.b "a"] (O.arrow a a))]
    , O.kindSig "Identity" (O.forallVisible [O.b "k"] (O.arrow k k))
    , O.closedFamily "Identity" [O.b "k", O.b "a"] [([k,a],a)]
    , O.classD [] "Next" [O.bk "a" (O.t "Type")] [O.sig "next" (O.arrow a a)]
    , O.instanceD (O.tc "Next" [O.t "Int"]) [O.val "next" [O.p "x"] (O.op "+" (O.v "x") (O.integer 1))]
    , O.sig "doubleM" (O.arrow (O.tc "M.Th" [O.t "Int"]) (O.tc "M.MemoristM" [O.t "Int"]))
    , O.val "doubleM" [O.p "tx"] $ O.doE
        [O.thenS (O.ann (O.v "M.tick") (O.tc "M.MemoristM" [O.t "()"])), O.bind "x" (O.call "M.force" [O.v "tx"])] $
        O.letE [O.val "y" [] (O.op "+" (O.v "x") (O.v "x"))] $
        O.caseE (O.call "Box" [O.v "y"])
            [(O.conP "Box" [O.p "result"], O.ret (O.v "result"))]
    , O.sig "program" (O.tc "M.MemoristM" [O.t "Int"])
    , O.val "program" [] (O.call "M.fromPure1" [O.v "doubleM", O.ann (O.integer 5) (O.t "Int")])
    , O.sig "converted" (O.tc "M.MemoristM" [O.t "Int"])
    , O.val "converted" [] (O.doE [O.bind "w" (O.ann (O.call "M.reprM" [O.call "Plain" [O.ann (O.integer 5) (O.t "Int")]]) (O.tc "M.MemoristM" [O.tc "Wrapped" [O.t "Int"]]))]
        (O.caseE (O.v "w") [(O.conP "Wrapped" [O.p "tx"], O.call "M.force" [O.v "tx"])]))
    , O.val "main" [] (O.call "print" [O.tuple
        [O.op "&&" (O.call "M.correct" [O.v "program", O.ann (O.integer 10) (O.t "Int")]) (O.call "M.correct" [O.v "converted", O.ann (O.integer 5) (O.t "Int")]),
         O.call "MT.toInteger" [O.call "M.infcost" [O.v "program", O.v "M.fullDemand"]]]])
    ]
  where a = O.t "a"; k = O.t "k"
