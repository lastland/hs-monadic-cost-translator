module Main (main) where

import Cost.Translate (translateFile)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [source, output] -> translateFile source output >>= either die pure
        _ -> die "usage: cost-translate SOURCE.hs OUTPUT.hs"
