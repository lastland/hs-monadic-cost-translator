module ClosedLocalComparisonChecks (checkClosedLocalComparisons) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import GeneratedConstruction (checkGeneratedConstruction)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

-- Each negative must first typecheck independently: a parser/type error is not
-- evidence that the construction boundary retained the forbidden obligation.
checkClosedLocalComparisons :: IO ()
checkClosedLocalComparisons = forM_ cases $ \(label, body, expected) ->
    withSystemTempDirectory ("closed-local-" ++ label) $ \directory -> do
        let path = directory </> "Generated.hs"
        writeFile path (unlines
            (["{-# LANGUAGE NoMonoLocalBinds, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}",
              "module Generated where", "import qualified Memorist.Memorist as M"] ++ body))
        (status, out, err) <- readProcessWithExitCode "ghc"
            ["-v0", "-fno-code", "-fforce-recomp", "-i", "-outputdir", directory,
             "-package", "MemoristANT", "-package", "QuickCheck", path] ""
        unless (status == ExitSuccess) (fail (label ++ ": must independently typecheck\n" ++ out ++ err))
        result <- try @IOException (checkGeneratedConstruction path)
        let diagnostics = either show (const "") result
        case expected of
            Nothing -> unless (null diagnostics) (fail (label ++ ": expected acceptance\n" ++ diagnostics))
            Just reason -> unless (reason `isInfixOf` diagnostics)
                (fail (label ++ ": missing " ++ reason ++ "\n" ++ diagnostics))
        putStrLn ("PASS: closed-local comparison / " ++ label)
  where
    eqFailure = "GHC.Classes.==: unknown concrete scalar instance"
    same = "let same x y = (M.tick :: M.MemoristM ()) >> pure (x == y)"
    cases =
        [("closed-int", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same (1::Int) 2"], Nothing)
        ,("closed-bool", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same True False"], Nothing)
        ,("closed-char", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same 'a' 'b'"], Nothing)
        ,("closed-unit", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same () ()"], Nothing)
        ,("closed-float", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same (1::Float) 2"], Nothing)
        ,("closed-double", ["root :: M.MemoristM Bool", "root = " ++ same ++ " in same (1::Double) 2"], Nothing)
        ,("three-closed-instances",
            ["root :: M.MemoristM Bool", "root = " ++ same ++ " in do",
             "  _ <- same (1::Int) 2", "  _ <- same (1::Integer) 2", "  same (1::Double) 2"], Nothing)
        ,("numeric-three-closed-instances",
            ["root :: M.MemoristM Bool",
             "root = let same x y = (M.tick :: M.MemoristM ()) >> pure (x == y + 0) in do",
             "  _ <- same (1::Int) 2", "  _ <- same (1::Integer) 2", "  same (1::Double) 2"], Nothing)
        ,("multiuse-unsupported-list",
            ["root :: M.MemoristM Bool", "root = " ++ same ++ " in do",
             "  _ <- same (1::Integer) 2", "  same [1::Int] []"], Just eqFailure)
        ,("multiuse-protected-thunk",
            ["root :: M.MemoristM Bool", "root = do",
             "  th <- M.lazy (pure (1::Int))", "  " ++ same ++ " in do",
             "    _ <- same (1::Integer) 2", "    same th th"], Just eqFailure)
        ,("raw-dead-protected-call",
            ["root :: M.MemoristM Bool", "root = do",
             "  th <- M.lazy (pure (1::Int))", "  " ++ same ++ " in",
             "    if True then same (1::Integer) 2 else same th th"], Just eqFailure)
        ,("protected-body-at-closed-int",
            ["root :: M.MemoristM Bool",
             "root = let helper x = do",
             "             (M.tick :: M.MemoristM ())",
             "             th <- M.lazy (pure x)",
             "             pure (th == th)",
             "       in helper (1::Int)"], Just "GHC.Classes.==: protected thunk/annotation/accounting operand")
        ,("unused-bad-helper",
            ["root :: M.MemoristM Bool", "root = let unused x y = (M.tick :: M.MemoristM ()) >> pure (x == y)",
             "           same x y = (M.tick :: M.MemoristM ()) >> pure (x == y)",
             "       in same (1::Integer) 2"], Just eqFailure)
        ,("open-unmarked-caller",
            ["root :: Eq a => a -> a -> M.MemoristM Bool",
             "root x y = " ++ same ++ " in same x y"], Just eqFailure)
        ,("open-numeric-is-not-scalar",
            ["root :: (Eq a, Num a) => a -> a -> M.MemoristM Bool",
             "root x y = let same u v = (M.tick :: M.MemoristM ()) >> pure (u == v + 0)",
             "           in same x y"], Just eqFailure)
        ,("opaque-callback-escape",
            ["opaque :: (Int -> Int -> M.MemoristM Bool) -> M.MemoristM Bool",
             "opaque callback = callback 1 2",
             "root :: M.MemoristM Bool", "root = " ++ same ++ " in opaque same"], Just eqFailure)
        ,("mixed-direct-and-opaque",
            ["opaque :: (Int -> Int -> M.MemoristM Bool) -> M.MemoristM Bool",
             "opaque callback = callback 1 2",
             "root :: M.MemoristM Bool", "root = " ++ same ++ " in do",
             "  _ <- same (1::Integer) 2", "  opaque same"], Just eqFailure)
        ,("dead-alias",
            ["root :: M.MemoristM Bool", "root = " ++ same ++ " in",
             "  let alias = same :: Int -> Int -> M.MemoristM Bool in same (1::Integer) 2"], Just eqFailure)
        ,("partial-call",
            ["root :: M.MemoristM Bool", "root = " ++ same ++ " in",
             "  let partial = same (1::Int) in partial 2"], Just eqFailure)
        ,("returned-callback",
            ["root :: M.MemoristM Bool",
             "root = let producer x = (M.tick :: M.MemoristM ()) >>",
             "                         pure (\\y -> (M.tick :: M.MemoristM ()) >> pure (x == y))",
             "       in do callback <- producer (1::Int)",
             "             callback 2"], Just eqFailure)
        ,("returned-newtype-callback",
            ["newtype Callback a = Callback (a -> M.MemoristM Bool)",
             "root :: M.MemoristM Bool",
             "root = let producer x = (M.tick :: M.MemoristM ()) >>",
             "                         pure (Callback (\\y -> (M.tick :: M.MemoristM ()) >> pure (x == y)))",
             "       in do Callback callback <- producer (1::Int)",
             "             callback 2"], Just eqFailure)
        ,("unrelated-class-not-authorized",
            ["class Other a where mystery :: a -> Bool",
             "instance Other Int where mystery _ = True",
             "root :: M.MemoristM Bool",
             "root = let helper x = (M.tick :: M.MemoristM ()) >> pure (mystery x) in helper (1::Int)"],
             Just "Generated.mystery: unknown executable method")
        ,("all-recursive-sibling-obligations",
            ["class Other a where mystery :: a -> Bool",
             "instance Other Int where mystery _ = True",
             "root :: M.MemoristM Bool",
             "root = let evenN n = (M.tick :: M.MemoristM ()) >>",
             "                      (if n == 0 then pure True else oddN (n-1))",
             "           oddN n = (M.tick :: M.MemoristM ()) >>",
             "                     (if n == 0 then pure (mystery n) else evenN (n-1))",
             "       in evenN (4::Int)"], Just "Generated.mystery: unknown executable method")
        ,("visible-observing-instance",
            ["instance {-# OVERLAPPABLE #-} Eq a where",
             "  _ == _ = M.tval (undefined :: M.Th Bool)",
             "root :: M.MemoristM Bool", "root = " ++ same ++ " in same (1::Int) 2"],
             Just "unapproved runtime operation Memorist.Base.Thunk.tval")
        ,("line-and-name-collision",
            ["{-# LINE 20 \"Same.hs\" #-}",
             "good :: M.MemoristM Bool", "good = " ++ same ++ " in same (1::Integer) 2",
             "{-# LINE 20 \"Same.hs\" #-}",
             "bad :: Eq a => a -> a -> M.MemoristM Bool", "bad x y = " ++ same ++ " in same x y"], Just eqFailure)
        ]
