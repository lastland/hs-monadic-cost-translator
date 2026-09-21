-- | Command-line shape: exactly two arguments, @SOURCE.hs OUTPUT.hs@; any
-- other argument count is a usage error. The executable is a thin wrapper
-- over 'Cost.Translate.translateFile', exiting nonzero on translation
-- failure.
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
