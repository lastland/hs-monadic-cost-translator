module Observe (main) where

import qualified Case
import Control.Monad (unless)
import ReferenceSupport (drawProbeDemands, runProbes)
import Protocol (Request(..), Response(..))
import System.IO (BufferMode(LineBuffering), hSetBuffering, isEOF, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    loop
  where
    loop = do
        finished <- isEOF
        unless finished $ do
            input <- getLine
            request <- maybe (fail "invalid comparison request") pure (readMaybe input)
            response <- case request of
                Draw budget seed sample -> Drawn <$> drawProbeDemands Case.probes budget seed sample
                Evaluate sample batches -> do
                    (results, demands) <- runProbes Case.probes sample batches
                    pure (Evaluated results demands)
            print response
            loop
