module Observe (main) where

import qualified Case
import Control.Monad (unless)
import ReferenceSupport (runProbes)
import System.IO (BufferMode(LineBuffering), hIsEOF, hSetBuffering, stdin, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    loop
  where
    loop = do
        finished <- hIsEOF stdin
        unless finished $ do
            input <- getLine
            sample <- maybe (fail "invalid comparison sample") pure (readMaybe input)
            results <- runProbes Case.probes sample
            print results
            loop
